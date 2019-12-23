
module AOC.Common.Intcode.Memory (
    MonadMem(..)
  , Memory(..)
  , mRegLens
  , MemRef(..)
  , initMemRef
  , freezeMemRef
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Conduino
import           Data.Generics.Labels         ()
import           Data.Map                     (Map)
import           Data.Primitive.MutVar
import           GHC.Generics
import           Numeric.Natural              (Natural)
import qualified Data.Map                     as M
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as MVS


class Monad m => MonadMem m where
    mRead      :: m Int
    mCurr      :: m Natural
    mPeek      :: Natural -> m Int
    mSeek      :: Natural -> m ()
    mWrite     :: Natural -> Int -> m ()
    mShiftBase :: Int -> m ()
    mWithBase  :: Int -> m Int

    -- mFreeze    :: m Memory
    -- mPutMem      :: Memory -> m ()


data Memory = Mem
    { mPos  :: Natural
    , mBase :: Int
    , mRegs :: Map Natural Int
    }
  deriving (Eq, Ord, Show, Generic)
instance NFData Memory

instance Monad m => MonadMem (StateT Memory m) where
    mRead = do
      m@Mem{..} <- get
      M.findWithDefault 0 mPos mRegs <$ put (m { mPos = mPos + 1 })
    mCurr = gets mPos
    mPeek i = gets $ M.findWithDefault 0 i . mRegs
    mSeek z = modify $ \m -> m { mPos = z }
    mWrite i x = modify $ \m -> m { mRegs = M.insert i x (mRegs m) }
    mShiftBase b = modify $ \m -> m { mBase = mBase m + b }
    mWithBase i = gets $ (+ i) . mBase

    -- mFreeze = get
    -- mPutMem   = put

instance MonadMem m => MonadMem (Pipe i o u m) where
    mRead = lift mRead
    mCurr = lift mCurr
    mPeek = lift . mPeek
    mSeek = lift . mSeek
    mWrite i = lift . mWrite i
    mShiftBase = lift . mShiftBase
    mWithBase  = lift . mWithBase
    -- mFreeze = lift mFreeze
    -- mPutMem   = lift . mPutMem

instance MonadMem m => MonadMem (ExceptT e m) where
    mRead = lift mRead
    mCurr = lift mCurr
    mPeek = lift . mPeek
    mSeek = lift . mSeek
    mWrite i = lift . mWrite i
    mShiftBase = lift . mShiftBase
    mWithBase  = lift . mWithBase
    -- mFreeze = lift mFreeze
    -- mPutMem   = lift . mPutMem

mRegLens :: Natural -> Lens' Memory Int
mRegLens i = #mRegs . at i . non 0

data MemRef s = MemRef
    { mrPos  :: MutVar s Natural
    , mrBase :: MutVar s Int
    , mrRegs :: MutVar s (VS.MVector s Int)
    }

initMemRef :: (PrimMonad m, s ~ PrimState m) => Memory -> m (MemRef s)
initMemRef Mem{..} = do
    mrPos  <- newMutVar mPos
    mrBase <- newMutVar mBase
    mrRegs <- case M.lookupMax mRegs of
      Nothing     -> newMutVar =<< MVS.new 0
      Just (n, _) -> do
        let r = VS.generate (fromIntegral n * 10 + 1) $ \i -> M.findWithDefault 0 (fromIntegral i) mRegs
        newMutVar =<< VS.thaw r
    pure MemRef{..}

freezeMemRef :: (PrimMonad m, s ~ PrimState m) => MemRef s -> m Memory
freezeMemRef MemRef{..} = do
    mPos  <- readMutVar mrPos
    mBase <- readMutVar mrBase
    mRegs <- fmap toRegs . VS.freeze =<< readMutVar mrRegs
    pure Mem{..}
  where
    toRegs = M.filter (/= 0) . M.fromList . zip [0..] . VS.toList


instance (PrimMonad m, s ~ PrimState m) => MonadMem (ReaderT (MemRef s) m) where
    mRead = ask >>= \MemRef{..} -> do
      i <- fromIntegral <$> atomicModifyMutVar' mrPos (\i -> (i+1, i))
      mPeek i
    mCurr = readMutVar =<< asks mrPos
    mPeek i = do
      r <- readMutVar =<< asks mrRegs
      if i' < MVS.length r
        then MVS.unsafeRead r i'
        else pure 0
      where
        i' = fromIntegral i
    mSeek i = (`writeMutVar` i) =<< asks mrPos
    mWrite i x = ask >>= \MemRef{..} -> do
        r <- readMutVar mrRegs
        let l0 = MVS.length r
        if i' < MVS.length r
          then MVS.unsafeWrite r i' x
          -- else trace "grow" $ do
          else do
            let l1 = (i' + 1) * 2
            regs' <- MVS.unsafeGrow r (l1 - l0)
            forM_ [l0 .. l1 - 1] $ \j ->
              MVS.unsafeWrite regs' j 0
            MVS.unsafeWrite regs' i' x
            writeMutVar mrRegs regs'
      where
        i' = fromIntegral i
    mShiftBase b = (`modifyMutVar'` (+ b)) =<< asks mrBase
    mWithBase  i = fmap (+ i) . readMutVar =<< asks mrBase

    -- mFreeze = freezeMemRef =<< ask
    -- mPutMem Mem{..} = ask >>= \MemRef{..} -> do
    --   writeMutVar mrPos  mPos
    --   writeMutVar mrBase mBase
    --   case M.lookupMax mRegs of
    --     Nothing     -> writeMutVar mrRegs =<< MVS.new 0
    --     Just (n, _) -> do
    --       let r = VS.generate (fromIntegral n + 1) $ \i -> M.findWithDefault 0 (fromIntegral i) mRegs
    --       writeMutVar mrRegs =<< VS.thaw r

