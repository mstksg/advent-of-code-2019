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

import           AOC.Common                (loopMaybeM)
import           AOC.Solver                ((:~>)(..))
import           AOC.Util                  (maybeToEither, eitherToMaybe)
import           Control.Applicative
import           Control.Monad             (void)
import           Control.Monad.Except      (MonadError(..))
import           Control.Monad.Loops
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Conduit              (ConduitT, await, yield, (.|), runConduit)
import           Data.Conduit.Lift
import           Data.List.Split           (splitOn)
import           Data.Map                  (Map)
import           Data.Sequence             (Seq)
import           Data.Traversable          (for, mapAccumL)
import           Linear                    (V0(..), V1(..), V2(..))
import           Safe                      (lastMay)
import           Text.Read                 (readMaybe)
import qualified Data.Conduit.Combinators  as C
import qualified Data.Map                  as M
import qualified Data.Sequence             as Seq

data Memory = Mem
    { mPos  :: Int
    , mRegs :: Seq Int
    }
  deriving Show

data Mode = Pos | Imm
  deriving (Eq, Ord, Enum, Show)

data Instr = Add | Mul | Get | Put | Jnz | Jez | Clt | Ceq | Hlt
  deriving (Eq, Ord, Enum, Show)

-- readMem :: Memory -> Maybe (Int, Memory)
-- readMem (Mem p r) = (, Mem (p + 1) r) <$> Seq.lookup p r

-- seekMem :: Int -> Memory -> Memory
-- seekMem i (Mem _ r) = Mem i r

instrMap :: Map Int Instr
instrMap = M.fromList $
    (99, Hlt) : zip [1 ..] [Add ..]

instr :: Int -> Maybe Instr
instr = (`M.lookup` instrMap) . (`mod` 100)

data InstrOp m = forall t. (Traversable t, Applicative t) => InstrOp (t Int -> ConduitT Int Int m InstrRes)

data InstrRes = IRWrite Int
              | IRJump  Int
              | IRNop
              | IRHalt
  deriving Show

instrOp
    :: MonadError String m
    => Instr
    -> InstrOp m
instrOp = \case
    Add -> InstrOp $ \case V2 x y -> pure . IRWrite $ x + y
    Mul -> InstrOp $ \case V2 x y -> pure . IRWrite $ x * y
    Get -> InstrOp $ \case V0     -> await >>= \case
                             Nothing -> throwError "no input";
                             Just x  -> pure $ IRWrite x
    Put -> InstrOp $ \case V1 x   -> IRNop <$ yield x
    Jnz -> InstrOp $ \case V2 x y -> pure $ if x /= 0 then IRJump y else IRNop
    Jez -> InstrOp $ \case V2 x y -> pure $ if x == 0 then IRJump y else IRNop
    Clt -> InstrOp $ \case V2 x y -> pure . IRWrite $ if x <  y then 1 else 0
    Ceq -> InstrOp $ \case V2 x y -> pure . IRWrite $ if x == y then 1 else 0
    Hlt -> InstrOp $ \case V0     -> pure IRHalt

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

runInstrOp
    :: (Traversable t, MonadError String m, MonadState Memory m)
    => t Mode
    -> (t Int -> m InstrRes)
    -> m InstrRes
runInstrOp modes f = do
    inp <- for modes $ \mode -> do
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

step
    :: (MonadError String m, MonadState Memory m)
    => ConduitT Int Int m Bool
step = do
    x <- readMem
    o <- maybeToEither "bad instr" $ instr x
    case instrOp o of
      InstrOp f -> do
        ir <- runInstrOp (fillModes (x `div` 100)) f
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

