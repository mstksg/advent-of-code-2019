
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
import           Data.Generics.Labels    ()
import           Data.Map                (Map)
import           Data.STRef
import           GHC.Generics
import           Numeric.Natural         (Natural)
import qualified Data.Map                as M
import qualified Data.Vector.Storable    as VS


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

mRegLens :: Natural -> Lens' Memory Int
mRegLens i = #mRegs . at i . non 0

data MemRef s = MemRef
    { mrPos  :: STRef s Natural
    , mrBase :: STRef s Int
    , mrRegs :: VS.MVector s Int
    }

-- instance (PrimMonad m, s ~ PrimState m) => MonadMem (ReaderT (MemRef s) m) where
--     mRead = asks >>= \MemRef{..} -> do

