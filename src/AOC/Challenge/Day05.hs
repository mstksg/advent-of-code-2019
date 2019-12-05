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

import           AOC.Common           (loopMaybeM)
import           AOC.Solver           ((:~>)(..))
import           AOC.Util             (maybeToEither, eitherToMaybe)
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.State  (MonadState(..), runStateT, evalStateT)
import           Control.Monad.Writer (MonadWriter(..), execWriterT)
import           Data.List.Split      (splitOn)
import           Data.Map             (Map)
import           Data.Sequence        (Seq)
import           Data.Traversable     (for, mapAccumL)
import           Linear               (V0(..), V1(..), V2(..))
import           Safe                 (lastMay)
import           Text.Read            (readMaybe)
import qualified Data.Map             as M
import qualified Data.Sequence        as Seq

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
instr = (`M.lookup` instrMap) . (`mod` 100)

data InstrOp m = forall t. (Traversable t, Applicative t) => InstrOp (t Int -> m InstrRes)

data InstrRes = IRWrite Int
              | IRJump  Int
              | IRNop
              | IRHalt
  deriving Show

instrOp
    :: (MonadState [Int] m, MonadWriter [Int] m, MonadError String m)
    => Instr
    -> InstrOp m
instrOp = \case
    Add -> InstrOp $ \case V2 x y -> pure . IRWrite $ x + y
    Mul -> InstrOp $ \case V2 x y -> pure . IRWrite $ x * y
    Get -> InstrOp $ \case V0     -> get >>= \case [] -> throwError "no input"; x:xs -> IRWrite x <$ put xs
    Put -> InstrOp $ \case V1 x   -> IRNop <$ tell [x]
    Jnz -> InstrOp $ \case V2 x y -> pure $ if x /= 0 then IRJump y else IRNop
    Jez -> InstrOp $ \case V2 x y -> pure $ if x == 0 then IRJump y else IRNop
    Clt -> InstrOp $ \case V2 x y -> pure . IRWrite $ if x <  y then 1 else 0
    Ceq -> InstrOp $ \case V2 x y -> pure . IRWrite $ if x == y then 1 else 0
    Hlt -> InstrOp $ \case V0     -> pure IRHalt

runInstrOp
    :: (Traversable t, MonadError String m)
    => Memory
    -> t Mode
    -> (t Int -> m InstrRes)
    -> m (InstrRes, Int)
runInstrOp (Mem p r) modes f = do
    (inp, p') <- flip runStateT (p + 1) . for modes $ \m -> do
      q <- tick
      a <- maybeToEither "bad index" $ Seq.lookup q r
      case m of
        Pos -> maybeToEither "bad index" $ Seq.lookup a r
        Imm -> pure a
    (,p') <$> f inp
  where
    tick = state $ \i -> (i, i + 1)

-- | Magically fills a fixed-shape 'Applicative' with each mode from a mode
-- op int.
fillModes :: (Traversable t, Applicative t) => Int -> t Mode
fillModes i = snd $ mapAccumL go i (pure ())
  where
    go j _ = (t, case o of 0 -> Pos; _ -> Imm)
      where
        (t,o) = j `divMod` 10

step
    :: (MonadWriter [Int] m, MonadState [Int] m, MonadError String m)
    => Memory
    -> m (Maybe Memory)
step m@(Mem p r) = do
    x <- maybeToEither "bad index" $ Seq.lookup p r
    o <- maybeToEither "bad instr" $ instr x
    case instrOp o of
      InstrOp f -> do
        (ir, q) <- runInstrOp m (fillModes (x `div` 100)) f
        case ir of
          IRWrite y -> do
            c <- maybeToEither "bad index" $ Seq.lookup q r
            pure . Just $ Mem (q + 1) (Seq.update c y r)
          IRJump  z ->
            pure . Just $ Mem z r
          IRNop     ->
            pure . Just $ Mem q r
          IRHalt    -> pure Nothing

runProg :: [Int] -> Memory -> Either String [Int]
runProg inp m = execWriterT $ evalStateT (loopMaybeM step m) inp

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

