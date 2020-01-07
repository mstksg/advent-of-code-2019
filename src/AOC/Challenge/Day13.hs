{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE TypeApplications   #-}

-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day13 (
    day13a
  , day13b
  , playDay13
  ) where

import           AOC.Common                (Point, displayAsciiMap)
import           AOC.Common.Intcode        (parseMem, Memory(..), VMErr, mRegLens, stepForever, untilHalt)
import           AOC.Solver                ((:~>)(..))
import           Control.Applicative       (empty)
import           Control.DeepSeq           (NFData)
import           Control.Lens              ((&), (.~), set)
import           Control.Monad             (join)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.State       (get, put, evalState)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Coerce               (coerce)
import           Data.Conduino             ((.|), Pipe, runPipe, yield, await)
import           Data.Foldable             (forM_)
import           Data.Map                  (Map)
import           Data.Monoid.OneLiner      (GMonoid(..))
import           Data.Semigroup            (Last(..), Max(..), Dual(..))
import           Data.Set                  (Set)
import           GHC.Generics              (Generic)
import           Linear.V2                 (V2(..))
import           Linear.V3                 (V3(..))
import qualified Data.Conduino.Combinators as C
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Graphics.Vty              as V

data Tile = Blank | Wall | Block | Paddle | Ball
  deriving (Eq, Ord, Enum, Generic)
instance NFData Tile

tileMap :: Map Int Tile
tileMap = M.fromList $ zip [0..] [Blank ..]

displayWith
    :: (Monoid o, Monad m)
    => (Either Int (Point, Tile) -> o)  -- ^ what do we want to aggregate?
    -> Pipe Int o u m ()
displayWith f = parseOutput
             .| C.map f
             .| C.scan (<>) mempty
  where
    parseOutput = do
      outs <- sequenceA $ V3 await await await
      forM_ (sequenceA outs) $ \(V3 x y z) -> do
        if (x, y) == (-1, 0)
          then yield (Left z)
          else forM_ (M.lookup z tileMap) $ \t ->
                 yield (Right (V2 x y, t))
        parseOutput

day13a :: Memory :~> Int
day13a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = fmap S.size . getTiles
    }
  where
    getTiles m = join . runPipe $
        empty
     .| untilHalt (stepForever @VMErr m)
     .| displayWith (\case Right (p, Block) -> S.singleton p
                           _                -> mempty
                    )
     .| C.last


day13b :: Memory :~> Int
day13b = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> do
        (Just scr, 0) <- ai $ set (mRegLens 0) 2 m
        pure scr
    }

data AI = AI { aiScore  :: !(Maybe (Last Int))
             , aiPaddle :: !(Maybe (Last Int))
             , aiBall   :: !(Maybe (Last Int))
             , aiBlocks :: !(Set Point)
             , aiBlanks :: !(Set Point)
             }
  deriving Generic
  deriving Semigroup via (GMonoid AI)
  deriving Monoid    via (GMonoid AI)

ai :: Memory -> Maybe (Maybe Int, Int)
ai m = flip evalState (Nothing, Nothing) . runPipe $
         C.repeatM controller
      .| untilHalt (stepForever @VMErr m)
      .| displayWith aggregator
      .| C.iterM (\AI{..} -> put . coerce $ ( aiPaddle, aiBall ))
      .| (fmap outScore <$> C.last)
  where
    controller = do
      (paddlePos, ballPos) <- get
      case (,) <$> paddlePos <*> ballPos of
        Nothing     -> pure 0
        Just (p, b) -> pure $ signum (b - p)
    aggregator = \case
      Left  s                -> mempty { aiScore  = Just (Last s) }
      Right (V2 x _, Paddle) -> mempty { aiPaddle = Just (Last x) }
      Right (V2 x _, Ball  ) -> mempty { aiBall   = Just (Last x) }
      Right (p     , Block ) -> mempty { aiBlocks = S.singleton p }
      Right (p     , Blank ) -> mempty { aiBlanks = S.singleton p }
      _                      -> mempty
    outScore AI{..} = (coerce aiScore, S.size $ aiBlocks S.\\ aiBlanks)


data Display = Disp
    { dispScore  :: !(Maybe (Max Int))
    , dispScreen :: !(Dual (Map Point Tile))
    }
  deriving Generic
  deriving Semigroup via (GMonoid Display)
  deriving Monoid    via (GMonoid Display)

playDay13 :: String -> IO ()
playDay13 str = do
    Just m <- pure $ parseMem str
    cfg    <- V.standardIOConfig
    vty    <- V.mkVty cfg
    disp   <- fmap join . runMaybeT . runPipe $
            C.repeatM (inputter vty)
         .| untilHalt (stepForever @VMErr (m & mRegLens 0 .~ 2))
         .| displayWith aggregator
         .| C.iterM (liftIO . V.update vty . V.picForImage . mkImage . render)
         .| C.last
    V.shutdown vty
    forM_ (dispScore =<< disp) $ \(Max s) ->
      putStrLn $ "final score: " ++ show s
  where
    inputter vty = do
      l <- liftIO $ V.nextEvent vty
      case l of
        V.EvKey V.KLeft       _ -> pure (-1)
        V.EvKey V.KRight      _ -> pure 1
        V.EvKey V.KEsc        _ -> empty
        V.EvKey (V.KChar 'a') _ -> pure (-1)
        V.EvKey (V.KChar 'd') _ -> pure 1
        V.EvKey (V.KChar 'q') _ -> empty
        _                       -> pure 0
    render Disp{..} = unlines
      [ displayAsciiMap ' ' $ fmap tileChar (coerce dispScreen)
      , case dispScore of
          Nothing      -> "No score"
          Just (Max s) -> show s
      , "←/a   left"
      , "↓/s   neutral"
      , "→/d   right"
      , "esc/q quit"
      ]
    aggregator = \case
      Left  s      -> mempty { dispScore  = Just (Max s)           }
      Right (p, t) -> mempty { dispScreen = Dual $ M.singleton p t }
    mkImage = V.vertCat . map (V.string mempty) . lines

tileChar :: Tile -> Char
tileChar = \case
    Blank  -> ' '
    Wall   -> '|'
    Block  -> '#'
    Paddle -> '-'
    Ball   -> 'o'

