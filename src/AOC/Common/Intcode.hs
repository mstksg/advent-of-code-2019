{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TupleSections             #-}


module AOC.Common.Intcode (
    Memory(..)
  , VM
  , stepForever
  , stepForeverAndDie
  , maybeToEither
  , runProg
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Conduino
import           Data.Conduino.Internal
import           Data.Map                  (Map)
import           Data.Sequence.NonEmpty    (NESeq(..))
import           Data.Traversable
import           Data.Void
import           Linear
import qualified Data.Conduino.Combinators as C
import qualified Data.Map                  as M
import qualified Data.Sequence.NonEmpty    as NESeq

type VM = Pipe Int Int Void (StateT Memory (Either String)) ()

data Memory = Mem
    { mPos  :: Int
    , mRegs :: NESeq Int
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
    case NESeq.lookup p r of
      Nothing -> throwError $ "bad index under current position: " ++ show p
      Just x  -> x <$ put (Mem (p + 1) r)

peekMem
    :: (MonadState Memory m, MonadError String m)
    => Int -> m Int
peekMem i = do
    Mem _ r <- get
    case NESeq.lookup i r of
      Nothing -> throwError $ "bad index for peek: " ++ show i
      Just x  -> pure x

-- useFuel :: (MonadState Memory m, MonadError String m) => m ()
-- useFuel = do
--     Mem u p r <- get
--     unless (u > 0) $ throwError "ran out of fuel"
--     put $ Mem (u - 1) p r

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
    => Pipe Int Int Void m Bool
step = do
    (mo, x) <- (`divMod` 100) <$> readMem
    o  <- maybeToEither ("bad instr: " ++ show x) $ instr x
    ir <- case o of
      Add -> withInput mo $ \case V2 a b  -> pure . IRWrite $ a + b
      Mul -> withInput mo $ \case V2 a b  -> pure . IRWrite $ a * b
      Get -> IRWrite <$> awaitSurely
      Put -> withInput mo $ \case V1 a    -> IRNop <$ yield a
      Jnz -> withInput mo $ \case V2 a b  -> pure $ if a /= 0 then IRJump b else IRNop
      Jez -> withInput mo $ \case V2 a b  -> pure $ if a == 0 then IRJump b else IRNop
      Clt -> withInput mo $ \case V2 a b  -> pure . IRWrite $ if a <  b then 1 else 0
      Ceq -> withInput mo $ \case V2 a b  -> pure . IRWrite $ if a == b then 1 else 0
      Hlt                                 -> pure IRHalt
    case ir of
      IRWrite y -> do
        c <- readMem
        True <$ modify (\(Mem p r) -> Mem p (NESeq.update c y r))
      IRJump  z ->
        True <$ modify (\(Mem _ r) -> Mem z r)
      IRNop     ->
        pure True
      IRHalt    ->
        pure False

stepForever
    :: (MonadState Memory m, MonadError String m)
    => Pipe Int Int Void m ()
stepForever = untilFalse step

stepForeverAndDie
    :: (MonadState Memory m, MonadError String m)
    => Pipe Int Int Void m Void
stepForeverAndDie = stepForever *> throwError "no more input to give"

untilFalse :: Monad m => m Bool -> m ()
untilFalse x = go
  where
    go = x >>= \case
      False -> pure ()
      True  -> go


runProg :: [Int] -> Memory -> Either String [Int]
runProg inp m = flip evalStateT m $
      runPipe $ (C.sourceList inp *> throwError "ran out of input")
             .| untilFalse step
             .| C.sinkList


-- data Memory = Mem
--     { mPos  :: !Int
--     , mRegs :: !(NESeq Int)
--     }
--   deriving Show

maybeToEither :: MonadError e m => e -> Maybe a -> m a
maybeToEither e = maybe (throwError e) pure

-- data Mode = Pos | Imm
--   deriving (Eq, Ord, Enum, Show)

-- data Instr = Add | Mul | Get | Put | Jnz | Jez | Clt | Ceq | Hlt
--   deriving (Eq, Ord, Enum, Show)

-- instrMap :: Map Int Instr
-- instrMap = M.fromList $
--     (99, Hlt) : zip [1 ..] [Add ..]

-- instr :: Int -> Maybe Instr
-- instr = (`M.lookup` instrMap) . (`mod` 100)

-- data InstrOp m = forall t. (Traversable t, Applicative t) => InstrOp (t Int -> Pipe Int Int Void m InstrRes)

-- data InstrRes = IRWrite Int
--               | IRJump  Int
--               | IRNop
--               | IRHalt
--   deriving Show

-- instrOp
--     :: MonadError String m
--     => Instr
--     -> InstrOp m
-- instrOp = \case
--     Add -> InstrOp $ \case V2 x y -> pure . IRWrite $ x + y
--     Mul -> InstrOp $ \case V2 x y -> pure . IRWrite $ x * y
--     Get -> InstrOp $ \case V0     -> await >>= \case
--                              Nothing -> throwError "no input";
--                              Just x  -> pure $ IRWrite x
--     Put -> InstrOp $ \case V1 x   -> IRNop <$ yield x
--     Jnz -> InstrOp $ \case V2 x y -> pure $ if x /= 0 then IRJump y else IRNop
--     Jez -> InstrOp $ \case V2 x y -> pure $ if x == 0 then IRJump y else IRNop
--     Clt -> InstrOp $ \case V2 x y -> pure . IRWrite $ if x <  y then 1 else 0
--     Ceq -> InstrOp $ \case V2 x y -> pure . IRWrite $ if x == y then 1 else 0
--     Hlt -> InstrOp $ \case V0     -> pure IRHalt

-- runInstrOp
--     :: (Traversable t, MonadError String m)
--     => Memory
--     -> t Mode
--     -> (t Int -> m InstrRes)
--     -> m (InstrRes, Int)
-- runInstrOp (Mem p r) modes f = do
--     (inp, p') <- flip runStateT (p + 1) . for modes $ \m -> do
--       q <- tick
--       a <- m2e "bad index" $ NESeq.lookup q r
--       case m of
--         Pos -> m2e "bad index" $ NESeq.lookup a r
--         Imm -> pure a
--     (,p') <$> f inp
--   where
--     tick = state $ \i -> (i, i + 1)

-- -- | Magically fills a fixed-shape 'Applicative' with each mode from a mode
-- -- op int.
-- fillModes :: (Traversable t, Applicative t) => Int -> t Mode
-- fillModes i = snd $ mapAccumL go i (pure ())
--   where
--     go j _ = (t, case o of 0 -> Pos; _ -> Imm)
--       where
--         (t,o) = j `divMod` 10

-- step
--     :: MonadError String m
--     => Memory
--     -> Pipe Int Int Void m (Maybe Memory)
-- step m@(Mem p r) = do
--     x <- m2e "bad index" $ NESeq.lookup p r
--     o <- m2e "bad instr" $ instr x
--     case instrOp o of
--       InstrOp f -> do
--         (ir, q) <- runInstrOp m (fillModes (x `div` 100)) f
--         case ir of
--           IRWrite y -> do
--             c <- m2e "bad index" $ NESeq.lookup q r
--             pure . Just $ Mem (q + 1) (NESeq.update c y r)
--           IRJump  z ->
--             pure . Just $ Mem z r
--           IRNop     ->
--             pure . Just $ Mem q r
--           IRHalt    -> pure Nothing

-- stepForever :: MonadError String m => Memory -> Pipe Int Int Void m Memory
-- stepForever = loopMaybeM step

-- loopMaybeM
--     :: Monad m
--     => (a -> m (Maybe a))
--     -> a
--     -> m a
-- loopMaybeM f = go
--   where
--     go !x = f x >>= \case
--       Nothing -> pure x
--       Just !y -> go y


