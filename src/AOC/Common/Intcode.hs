{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE TupleSections             #-}


module AOC.Common.Intcode (
    Memory(..)
  , mRegLens
  , VM
  , AsciiVM
  , stepForever
  , stepForeverAndDie
  , stepForeverMut
  , untilHalt
  , parseMem
  , untilFalse
  , step
  , stepN
  , yieldAndDie
  , yieldAndPass
  , VMErr(..)
  , IErr(..)
  , AsVMErr(..)
  , AsIErr(..)
  , toAsciiVM
  , preAscii, postAscii
  , interactVM
  , interactAsciiVM
  ) where

import           AOC.Common
import           AOC.Common.Intcode.Memory
import           AOC.Util
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens
import           Control.Monad.Error.Lens
import           Control.Monad.Except
import           Control.Monad.Primitive
import           Data.Char
import           Data.Conduino
import           Data.Conduino.Lift
import           Data.Generics.Labels      ()
import           Data.List.Split
import           Data.Map                  (Map)
import           Data.Text                 (Text)
import           Data.Traversable
import           Data.Typeable
import           Data.Void
import           GHC.Generics
import           GHC.Natural
import           Linear
import           Numeric.Natural           (Natural)
import           Text.Read                 (readMaybe)
import qualified Data.Conduino.Combinators as C
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T

type VM = Pipe Int Int Void

type AsciiVM = Pipe Text Text Void


data Mode = Pos | Imm | Rel
  deriving (Eq, Ord, Enum, Show, Generic)
instance NFData Mode

data Instr = Add | Mul | Get | Put | Jnz | Jez | Clt | Ceq | ChB | Hlt
  deriving (Eq, Ord, Enum, Show, Generic)
instance NFData Instr

data VMErr = VMEBadMode  Int
           | VMEBadInstr Int
           | VMEBadPos   Int
  deriving (Eq, Ord, Show, Typeable, Generic)
instance Exception VMErr
makeClassyPrisms ''VMErr

data IErr = IENoInput
          | IEVM VMErr
  deriving (Eq, Ord, Show, Typeable, Generic)
instance Exception IErr
makeClassyPrisms ''IErr

instance AsVMErr IErr where
    _VMErr = _IEVM

instrMap :: Map Int Instr
instrMap = M.fromList $
    (99, Hlt) : zip [1 ..] [Add ..]

instr :: Int -> Maybe Instr
instr = (`M.lookup` instrMap)

toNatural' :: (AsVMErr e, MonadError e m) => Int -> m Natural
toNatural' x = maybe (throwing _VMErr (VMEBadPos x)) pure
             . toNatural
             $ x

-- | Defered version of 'withInput', to allow for maximum 'laziness'.
withInputLazy
    :: (Traversable t, Applicative t, AsVMErr e, MonadError e m, MonadMem m)
    => Int      -- ^ mode int
    -> (t (m Int) -> m r)
    -> m (r, Mode)
withInputLazy mo f = do
    (lastMode, modes) <- case fillModes mo of
      Left  i -> throwing _VMErr $ VMEBadMode i
      Right x -> pure x
    inp <- for modes $ \mode -> do
      a <- mRead
      pure $ case mode of
        Pos -> do
          x <- toNatural' a
          mPeek x
        Imm -> pure a
        Rel -> do
          x <- toNatural' =<< mWithBase a
          mPeek x
    (, lastMode) <$> f inp

-- | Run a @t Int -> m r@ function by fetching an input container
-- @t Int@. This fetches everything in advance, so is 'strict'.
withInput
    :: (Traversable t, Applicative t, MonadMem m, AsVMErr e, MonadError e m)
    => Int      -- ^ mode int
    -> (t Int -> m r)
    -> m (r, Mode)
withInput mo f = withInputLazy mo ((f =<<) . sequenceA)

intMode :: Int -> Maybe Mode
intMode = \case 0 -> Just Pos
                1 -> Just Imm
                2 -> Just Rel
                _ -> Nothing


-- | Magically fills a fixed-shape 'Applicative' with each mode from a mode
-- op int.
fillModes :: forall t. (Traversable t, Applicative t) => Int -> Either Int (Mode, t Mode)
fillModes i = do
    (lastMode, ms) <- traverse sequence $ mapAccumL go i (pure ())
    (,ms) <$> maybeToEither lastMode (intMode lastMode)
  where
    go j _ = (t, maybeToEither o $ intMode o)
      where
        (t,o) = j `divMod` 10

-- | Useful type to abstract over the actions of the different operations
data InstrRes = IRWrite Int         -- ^ write a value to location at
              | IRJump  Natural     -- ^ jump to position
              | IRBase  Int         -- ^ set base
              | IRNop               -- ^ do nothing
              | IRHalt              -- ^ halt
  deriving (Eq, Ord, Show, Generic)

step
    :: (AsVMErr e, MonadError e m, MonadMem m)
    => Pipe Int Int Void m Bool
step = do
    (mo, x) <- (`divMod` 100) <$> mRead
    o  <- maybe (throwing _VMErr (VMEBadInstr x)) pure $
            instr x
    (ir, lastMode) <- case o of
      Add -> withInput     mo $ \case V2 a b -> pure . IRWrite $ a + b
      Mul -> withInput     mo $ \case V2 a b -> pure . IRWrite $ a * b
      Get -> withInput     mo $ \case V0     -> IRWrite <$> awaitSurely
      Put -> withInput     mo $ \case V1 a   -> IRNop <$ yield a
      Jnz -> withInputLazy mo $ \case V2 a b -> a >>= \case 0 -> pure IRNop
                                                            _ -> IRJump <$> (toNatural' =<< b)
      Jez -> withInputLazy mo $ \case V2 a b -> a >>= \case 0 -> IRJump <$> (toNatural' =<< b)
                                                            _ -> pure IRNop
      Clt -> withInput     mo $ \case V2 a b -> pure . IRWrite $ if a <  b then 1 else 0
      Ceq -> withInput     mo $ \case V2 a b -> pure . IRWrite $ if a == b then 1 else 0
      ChB -> withInput     mo $ \case V1 a   -> pure $ IRBase a
      Hlt -> withInput     mo $ \case V0     -> pure IRHalt
    case ir of
      IRWrite y -> do
        c <- toNatural' =<< case lastMode of
          Pos -> mPeek =<< mCurr
          Imm -> fromIntegral <$> mCurr
          Rel -> mWithBase =<< mPeek =<< mCurr
        _ <- mRead
        True <$ mWrite c y
      IRJump  z ->
        True <$ mSeek z
      IRBase  b ->
        True <$ mShiftBase b
      IRNop     ->
        pure True
      IRHalt    ->
        pure False

stepForever
    :: (AsVMErr e, MonadError e m)
    => Memory
    -> Pipe Int Int Void m Memory
stepForever m = execStateP m (untilFalse step)

stepForeverMut
    :: (AsVMErr e, MonadError e m, PrimMonad m)
    => Memory
    -> Pipe Int Int Void m Memory
stepForeverMut m = do
    mr <- lift $ initMemRef m
    runReaderP mr (untilFalse step)
    lift $ freezeMemRef mr

stepForeverAndDie
    :: MonadError IErr m
    => Memory
    -> Pipe Int Int Void m Void
stepForeverAndDie m = stepForever m *> throwError IENoInput

untilHalt
    :: Monad m
    => Pipe i o u (ExceptT e m) a
    -> Pipe i o u m             ()
untilHalt = runExceptP_

parseMem :: String -> Maybe Memory
parseMem = fmap (Mem 0 0 . M.fromList . zip [0..])
         . traverse readMaybe
         . splitOn ","

untilFalse :: Monad m => m Bool -> m ()
untilFalse x = go
  where
    go = x >>= \case
      False -> pure ()
      True  -> go

stepN
    :: (AsVMErr e, MonadError e m)
    => Natural
    -> Memory
    -> Pipe Int Int Void m (Maybe Natural)
stepN n m = evalStateP m (untilFalseN n step)

-- | Returns the 'fuel' remaining
untilFalseN :: Monad m => Natural -> m Bool -> m (Maybe Natural)
untilFalseN n x = go n
  where
    go i = case i `minusNaturalMaybe` 1 of
      Nothing -> pure Nothing
      Just j  -> x >>= \case
        False -> pure (Just j)
        True  -> go j

yieldAndDie :: MonadError IErr m => o -> Pipe i o u m a
yieldAndDie i = yield i *> throwError IENoInput

yieldAndPass :: o -> Pipe o o u m u
yieldAndPass i = yield i *> C.map id

preAscii :: Pipe Text Int u m u
preAscii = C.concatMap $ map ord . T.unpack . (<> "\n")

postAscii :: Monad m => Pipe Int Text u m u
postAscii = C.map chr .| C.mapAccum go [] .| C.concat
  where
    go c xs
      | c == '\n' = ([], Just (T.pack (reverse xs)))
      | otherwise = (c:xs, Nothing)

toAsciiVM :: Monad m => VM m a -> AsciiVM m a
toAsciiVM p = preAscii .| p .| postAscii

interactAsciiVM
    :: MonadIO m
    => AsciiVM (ExceptT IErr m) a
    -> m ()
interactAsciiVM vm = void . runPipe . untilHalt $
      (C.stdinLines *> throwError IENoInput)
   .| C.map T.pack
   .| vm
   .| C.map (T.encodeUtf8 . (<> "\n"))
   .| C.stdout

interactVM :: Memory -> IO ()
interactVM = interactAsciiVM . toAsciiVM . stepForever
