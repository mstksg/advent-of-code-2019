{-# LANGUAGE TypeApplications #-}

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

import           AOC.Common                (Point, displayAsciiMap, firstJust, countTrue)
import           AOC.Common.Conduino       (iterM)
import           AOC.Common.Intcode        (parseMem, Memory(..), IErr, mRegLens, stepForever, untilHalt)
import           AOC.Solver                ((:~>)(..))
import           Control.Applicative       ((<|>))
import           Control.DeepSeq           (NFData)
import           Control.Lens              ((&), (.~), view, set)
import           Control.Monad             (guard)
import           Control.Monad.State       (get, put, evalState)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Conduino             ((.|), Pipe, runPipe, runPipePure, yield, await)
import           Data.Foldable             (forM_)
import           Data.Functor              (void)
import           Data.Map                  (Map)
import           GHC.Generics              (Generic)
import           Linear.V2                 (V2(..), _x)
import qualified Data.Conduino.Combinators as C
import qualified Data.Map                  as M
import qualified Graphics.Vty              as V

data Tile = Empt | Wall | Block | Paddle | Ball
  deriving (Eq, Ord, Enum, Generic)
instance NFData Tile

tileMap :: Map Int Tile
tileMap = M.fromList $ zip [0..] [Empt ..]

data Display = Disp
    { dispScore  :: !(Maybe Int)
    , dispScreen :: !(Map Point Tile)
    }

instance Semigroup Display where
    Disp o1 e1 <> Disp o2 e2 = Disp (o2 <|> o1) (e2 <> e1)
instance Monoid Display where
    mempty = Disp Nothing M.empty

display :: Monad m => Pipe Int Display u m ()
display = void (runMaybeT parseOutput)
       .| C.scan buildDisplay (Disp Nothing M.empty)
  where
    parseOutput = do
      x <- MaybeT await
      y <- MaybeT await
      z <- MaybeT await
      if (x, y) == (-1, 0)
        then lift . yield $ Left z
        else forM_ (M.lookup z tileMap) $ \t ->
               lift . yield $ Right (V2 x y, t)
      parseOutput
    buildDisplay d = \case
      Left i      -> d { dispScore  = Just i }
      Right (p,t) -> d { dispScreen = M.insert p t (dispScreen d) }

day13a :: Memory :~> Int
day13a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = Just . countTrue (== Block) . getTiles
    }
  where
    getTiles m = runPipePure $ C.repeat 0
                            .| untilHalt (stepForever @IErr m)
                            .| display
                            .| (dispScreen <$> C.fold)


day13b :: Memory :~> Int
day13b = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> do
        (Just scr, 0) <- ai $ set (mRegLens 0) 2 m
        pure scr
    }

ai :: Memory -> Maybe (Maybe Int, Int)
ai m = flip evalState (Nothing, Nothing) . runPipe $
         C.repeatM controller
      .| untilHalt (stepForever @IErr m)
      .| display
      .| iterM (put . committer)
      .| (fmap getOut <$> C.last)
  where
    controller = do
      (paddlePos, ballPos) <- get
      case (,) <$> paddlePos <*> ballPos of
        Nothing     -> pure 0
        Just (p, b) -> pure $ signum (b - p)
    getOut (Disp scr mp) = (scr, countTrue (== Block) mp)
    committer (Disp _ mp) = ( view _x <$> findItem Paddle
                            , view _x <$> findItem Ball
                            )
      where
        findItem c = firstJust (\(k,v) -> k <$ guard (c == v))
                   $ M.toList mp

playDay13 :: String -> IO ()
playDay13 str = do
    Just m <- pure $ parseMem str
    cfg    <- V.standardIOConfig
    vty    <- V.mkVty cfg
    runPipe $ C.repeatM (inputter vty)
           .| untilHalt (stepForever @IErr (m & mRegLens 0 .~ 2))
           .| display
           .| C.map render
           .| C.mapM (V.update vty . V.picForImage . mkImage)
           .| C.sinkNull
    V.shutdown vty
  where
    inputter vty = do
      l <- V.nextEvent vty
      case l of
        V.EvKey V.KLeft _ -> pure (-1)
        V.EvKey V.KDown _ -> pure 0
        V.EvKey V.KRight _ -> pure 1
        _              -> pure 0
    render Disp{..} = unlines
      [ displayAsciiMap ' ' $ fmap tileChar dispScreen
      , case dispScore of
          Nothing -> "No score"
          Just s  -> show s
      ]
    mkImage = V.vertCat . map (V.string mempty) . lines

tileChar :: Tile -> Char
tileChar = \case
    Empt -> ' '
    Wall -> '|'
    Block -> '#'
    Paddle -> '-'
    Ball -> 'o'

