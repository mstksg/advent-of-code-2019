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
  , iterM
  , feedPipe
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Free hiding (iterM)
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Conduino
import           Data.Conduino.Internal
import           Data.Sequence                   (Seq(..))
import qualified Data.Conduino.Combinators       as C
import qualified Data.Sequence                   as Seq

-- | Loop a pipe into itself.
--
-- *  Will feed all output back to the input
-- *  Will only ask for input upstream if output is stalled.
-- *  Yields all outputted values downstream, effectively duplicating them.
feedbackP
    :: Monad m
    => Pipe x x u m a
    -> Pipe x x u m a
feedbackP p = evalStateP Seq.empty $
       popper
    .| hoistPipe lift p
    .| iterM (\x -> modify (:|> x))
  where
    popper = lift get >>= \case
      Empty -> awaitEither >>= \case
        Left r  -> pure r
        Right x -> yield x >> popper
      x :<| xs -> do
        lift $ put xs
        yield x
        popper

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

iterM :: Monad m => (i -> m ()) -> Pipe i i u m u
iterM f = C.mapM (\x -> x <$ f x)

feedPipe
    :: Monad m
    => [i]
    -> Pipe i o u m a
    -> m ([o], Either (i -> Pipe i o u m a) a)
feedPipe xs = (fmap . second . first . fmap) fromRecPipe . feedPipe_ xs . toRecPipe

feedPipe_
    :: Monad m
    => [i]
    -> RecPipe i o u m a
    -> m ([o], Either (i -> RecPipe i o u m a) a)
feedPipe_ xs (FreeT p) = p >>= \case
    Pure y -> pure ([], Right y)
    Free (PAwaitF _ g) -> case xs of
      []   -> pure ([], Left g)
      y:ys -> feedPipe_ ys (g y)
    Free (PYieldF o q) -> first (o:) <$> feedPipe_ xs q

