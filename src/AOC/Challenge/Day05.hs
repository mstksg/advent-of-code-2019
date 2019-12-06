{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day05 (
    day05a
  , day05b
  , fillModes
  ) where

import           AOC.Solver               ((:~>)(..))
import           AOC.Util                 (maybeToEither, eitherToMaybe)
import           Control.Monad            (void)
import           Control.Monad.Except     (MonadError, throwError)
import           Control.Monad.State      (MonadState, get, put, modify)
import           Data.Conduit             (ConduitT, await, yield, (.|), runConduit)
import           Data.Conduit.Lift        (runStateC)
import           Data.List.Split          (splitOn)
import           Data.Map                 (Map)
import           Data.Sequence            (Seq)
import           Data.Traversable         (for, mapAccumL)
import           Linear                   (V1(..), V2(..))
import           Safe                     (lastMay)
import           Text.Read                (readMaybe)
import qualified Data.Conduit.Combinators as C
import qualified Data.Map                 as M
import qualified Data.Sequence            as Seq

data Memory = Mem
    { mPos  :: Int
    , mRegs :: Seq Int
    }
  deriving Show

data Mode = Pos | Imm
  deriving (Eq, Ord, Enum, Show)

data Instr = Add | Mul | Get | Put | Jnz | Jez | Clt | Ceq | Hlt
  deriving (Eq, Ord, Enum, Show)

instrMap :: Map Int Instr
instrMap = M.fromList $
    (99, Hlt) : zip [1 ..] [Add ..]

instr :: Int -> Maybe Instr
instr = (`M.lookup` instrMap)

readMem
    :: (MonadState Memory m, MonadError String m)
    => m Int
readMem = do
    Mem p r <- get
    case Seq.lookup p r of
      Nothing -> throwError "bad index"
      Just x  -> x <$ put (Mem (p + 1) r)

peekMem
    :: (MonadState Memory m, MonadError String m)
    => Int -> m Int
peekMem i = do
    Mem _ r <- get
    case Seq.lookup i r of
      Nothing -> throwError "bad index"
      Just x  -> pure x

-- | Run a @t Int -> m r@ function by getting fetching an input container
-- @t Int@.
withInput
    :: (Traversable t, Applicative t, MonadState Memory m, MonadError String m)
    => Int      -- ^ mode int
    -> (t Int -> m r)
    -> m r
withInput mo f = do
    inp <- for (fillModes mo) $ \mode -> do
      a <- readMem
      case mode of
        Pos -> peekMem a
        Imm  -> pure a
    f inp

-- | Magically fills a fixed-shape 'Applicative' with each mode from a mode
-- op int.
fillModes :: (Traversable t, Applicative t) => Int -> t Mode
fillModes i = snd $ mapAccumL go i (pure ())
  where
    go j _ = (t, case o of 0 -> Pos; _ -> Imm)
      where
        (t,o) = j `divMod` 10

-- | Useful type to abstract over the actions of the different operations
data InstrRes = IRWrite Int         -- ^ write a value
              | IRJump  Int         -- ^ jump to position
              | IRNop               -- ^ do nothing
              | IRHalt              -- ^ halt
  deriving Show

step
    :: (MonadError String m, MonadState Memory m)
    => ConduitT Int Int m Bool
step = do
    (mo, x) <- (`divMod` 100) <$> readMem
    o  <- maybeToEither "bad instr" $ instr x
    ir <- case o of
      Add -> withInput mo $ \case V2 a b  -> pure . IRWrite $ a + b
      Mul -> withInput mo $ \case V2 a b  -> pure . IRWrite $ a * b
      Get -> await      >>= \case Nothing -> throwError "no input"
                                  Just a  -> pure $ IRWrite a
      Put -> withInput mo $ \case V1 a    -> IRNop <$ yield a
      Jnz -> withInput mo $ \case V2 a b  -> pure $ if a /= 0 then IRJump b else IRNop
      Jez -> withInput mo $ \case V2 a b  -> pure $ if a == 0 then IRJump b else IRNop
      Clt -> withInput mo $ \case V2 a b  -> pure . IRWrite $ if a <  b then 1 else 0
      Ceq -> withInput mo $ \case V2 a b  -> pure . IRWrite $ if a == b then 1 else 0
      Hlt                                 -> pure IRHalt
    case ir of
      IRWrite y -> do
        c <- readMem
        True <$ modify (\(Mem p r) -> Mem p (Seq.update c y r))
      IRJump  z ->
        True <$ modify (\(Mem _ r) -> Mem z r)
      IRNop     ->
        pure True
      IRHalt    ->
        pure False

runProg :: [Int] -> Memory -> Either String [Int]
runProg inp m = runConduit $ C.yieldMany inp
                          .| void (runStateC m (untilFalse step))
                          .| C.sinkList

untilFalse :: Monad m => m Bool -> m ()
untilFalse x = go
  where
    go = x >>= \case
      False -> pure ()
      True  -> go
    

day05a :: Memory :~> Int
day05a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = (=<<) lastMay . eitherToMaybe . runProg [1]
    }

day05b :: Memory :~> Int
day05b = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = (=<<) lastMay . eitherToMaybe . runProg [5]
    }

parseMem :: String -> Maybe Memory
parseMem = fmap (Mem 0 . Seq.fromList)
         . traverse readMaybe
         . splitOn ","

