
module AOC.Common.Intcode.Memory (
    MonadMem(..)
  , Memory(..)
  , mRegLens
  , MemRef(..)
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.ST
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

    mFreeze    :: m Memory
    mInit      :: Memory -> m ()


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

    mFreeze = get
    mInit   = put

instance MonadMem m => MonadMem (Pipe i o u m) where
    mRead = lift mRead
    mCurr = lift mCurr
    mPeek = lift . mPeek
    mSeek = lift . mSeek
    mWrite i = lift . mWrite i
    mShiftBase = lift . mShiftBase
    mWithBase  = lift . mWithBase
    mFreeze = lift mFreeze
    mInit   = lift . mInit

instance MonadMem m => MonadMem (ExceptT e m) where
    mRead = lift mRead
    mCurr = lift mCurr
    mPeek = lift . mPeek
    mSeek = lift . mSeek
    mWrite i = lift . mWrite i
    mShiftBase = lift . mShiftBase
    mWithBase  = lift . mWithBase
    mFreeze = lift mFreeze
    mInit   = lift . mInit

mRegLens :: Natural -> Lens' Memory Int
mRegLens i = #mRegs . at i . non 0

data MemRef s = MemRef
    { mrPos  :: MutVar s Natural
    , mrBase :: MutVar s Int
    , mrRegs :: VS.MVector s Int
    }

-- instance (PrimMonad m, s ~ PrimState m) => MonadMem (ReaderT (MemRef s) m) where
--     mRead = ask >>= \MemRef{..} -> do
--       i <- fromIntegral <$> atomicModifyMutVar' mrPos (\i -> (i+1, i))
--       mPeek i
--     mCurr = readMutVar =<< asks mrPos
--     mPeek i = do
--       r <- asks mrRegs
--       if i' < MVS.length r
--         then MVS.unsafeRead r i'
--         else pure 0
--       where
--         i' = fromIntegral i
--     mSeek i = (`writeMutVar` i) =<< asks mrPos
--     -- mWrite i x = asks >>= \MemRef{..} -> do
--     --     if i' < MVS.length mRegs
--     --       then MVS.unsafeWrite mRegs i' x
--     --       else do

    --   where
    --     i' = fromIntegral i

        
      

