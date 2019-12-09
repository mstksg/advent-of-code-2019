{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans    #-}

module AOC.Common.Conduino (
    feedbackP
  , runStateP
  , execStateP
  , evalStateP
  , runExceptP
  , fuseBoth
  , fuseUpstream
  , (&|)
  , (|.)
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.State
import           Data.Conduino
import           Data.Conduino.Internal
import           Data.Sequence              (Seq(..))
import qualified Data.Sequence              as Seq

-- | Loop a pipe into itself.
--
-- *  Will feed all output back to the input
-- *  Will only ask for input upstream if output is stalled.
-- *  Yields all outputted values downstream, effectively duplicating them.
feedbackP
    :: (Monad m, Show x)
    => Pipe x x u m a
    -> Pipe x x u m a
feedbackP = fromRecPipe . feedbackP_ Seq.empty . toRecPipe

feedbackP_ :: (Monad m, Show x) => Seq x -> RecPipe x x u m a -> RecPipe x x u m a
feedbackP_ inp (FreeT p) = FreeT $ p >>= \case
    Pure x             -> pure $ Pure x
    Free (PAwaitF f g) -> case inp of
      Empty    -> pure $ Free (PAwaitF (feedbackP_ inp . f) (feedbackP_ inp . g))
      i :<| is -> runFreeT $ feedbackP_ is (g i)
    Free (PYieldF o q) -> pure $ Free $ PYieldF o (feedbackP_ (inp :|> o) q)

execStateP :: Monad m => s -> Pipe i o u (StateT s m) a -> Pipe i o u m s
execStateP s = fmap snd . runStateP s

evalStateP :: Monad m => s -> Pipe i o u (StateT s m) a -> Pipe i o u m a
evalStateP s = fmap fst . runStateP s

runStateP :: Monad m => s -> Pipe i o u (StateT s m) a -> Pipe i o u m (a, s)
runStateP s = fromRecPipe . runStateP_ s . toRecPipe

runStateP_ :: Monad m => s -> RecPipe i o u (StateT s m) a -> RecPipe i o u m (a, s)
runStateP_ s (FreeT p) = FreeT $ do
    (q, s') <- runStateT p s
    case q of
      Pure x -> pure $ Pure (x, s')
      Free l -> pure $ Free (fmap (runStateP_ s') l)

runExceptP :: Monad m => Pipe i o u (ExceptT e m) a -> Pipe i o u m (Either e a)
runExceptP = fromRecPipe . runExceptP_ . toRecPipe

runExceptP_ :: Monad m => RecPipe i o u (ExceptT e m) a -> RecPipe i o u m (Either e a)
runExceptP_ (FreeT p) = FreeT $ runExceptT p >>= \case
    Left  e -> pure . Pure $ Left e
    Right (Pure x) -> pure . Pure $ Right x
    Right (Free l) -> pure $ Free (fmap runExceptP_ l)

fuseBoth :: Monad m => Pipe a b u m v -> Pipe b c v m r -> Pipe a c u m (v, r)
fuseBoth p q = p .| (q >>= exhaust)
  where
    exhaust x = go
      where
        go = awaitEither >>= \case
          Left  y -> pure (y, x)
          Right _ -> go

fuseUpstream
    :: Monad m
    => Pipe a b u m v
    -> Pipe b c v m r
    -> Pipe a c u m v
fuseUpstream p q = fst <$> fuseBoth p q

(&|) :: Monad m => Pipe a b u m v -> Pipe b c v m r -> Pipe a c u m (v, r)
(&|) = fuseBoth

(|.) :: Monad m => Pipe a b u m v -> Pipe b c v m r -> Pipe a c u m v
(|.) = fuseUpstream

infixr 2 &|
infixr 2 |.

deriving instance MonadPlus m => Alternative (Pipe a c u m)
deriving instance MonadPlus m => MonadPlus (Pipe a c u m)
