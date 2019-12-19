{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
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
  , stepPipe
  , PipeStep(..)
  , squeezePipe
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Free hiding (iterM)
import           Control.Monad.Trans.Free.Church hiding (iterM)
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

-- feedPipe
--     :: forall i o u m a. Monad m
--     => [i]
--     -> Pipe i o u m a
--     -> m ([o], Either (Either u i -> Pipe i o u m a) ([i], a))
-- feedPipe xs0 (Pipe p) = reshuffle =<< runStateT (runFT (hoistFT lift p) pr fr) xs0
--   where
--     reshuffle
--         :: (([o], Either (Either u i -> Pipe i o u (StateT [i] m) a) a), [i])
--         -> m ([o], Either (Either u i -> Pipe i o u m a) ([i], a))
--     reshuffle ((os, res), leftover) = case res of
--       Right x -> pure ((os, Right (leftover, x)))
--       Left  n -> do
--         runStateP leftover
--     -- reshuffle ((os, res), leftover) = (os, (leftover,) <$> res)
--     pr = pure . ([],) . Right
--     fr :: (x -> StateT [i] m ([o], Either (Either u i -> Pipe i o u (StateT [i] m) a) a))
--        -> PipeF i o u x
--        -> StateT [i] m ([o], Either (Either u i -> Pipe i o u (StateT [i] m) a) a)
--     fr pNext = \case
--       PAwaitF f g -> get >>= \case
--         []   -> pure . ([],) . Left $ (unFr =<<) . lift @(Pipe i o u) . pNext . either f g
--         x:xs -> put xs *> pNext (g x)
--       -- \case
--         -- [] -> pure . ([],) . Left $ (unFeed =<<) . pNext . either f g
--         -- pure . ([],) . Left $ (unSqueeze =<<) . lift . pNext . either f g
--       PYieldF o x -> first (o:) <$> pNext x
--     -- unFr :: Pipe i o u (StateT [i] m) ([o], Either (Either u i -> Pipe i o u m a) a)
--     --      -> Pipe i o u m a
--     unFr (os, next) = do
--       mapM_ yield os
--       case next of
--         Left  n -> n =<< awaitEither
--         Right x -> pure x

feedPipe
    :: Monad m
    => [i]
    -> Pipe i o u m a
    -> m ([o], Either (i -> Pipe i o u m a) ([i], a))
feedPipe xs = (fmap . second . first . fmap) fromRecPipe . feedPipe_ xs . toRecPipe

feedPipe_
    :: Monad m
    => [i]
    -> RecPipe i o u m a
    -> m ([o], Either (i -> RecPipe i o u m a) ([i], a))
feedPipe_ xs (FreeT p) = p >>= \case
    Pure y -> pure ([], Right (xs, y))
    Free (PAwaitF _ g) -> case xs of
      []   -> pure ([], Left g)
      y:ys -> feedPipe_ ys (g y)
    Free (PYieldF o q) -> first (o:) <$> feedPipe_ xs q

-- -- | For some reason this is much worse than the recPipe based feedPipe.
-- -- it might be becasue of 'runFT' being used repeatedly/recursively at
-- -- every step, whereas feedPipe uses it only once.
-- feedPipe
--     :: Monad m
--     => [i]
--     -> Pipe i o u m a
--     -> m ([o], Either (i -> Pipe i o u m a) ([i], a))
-- feedPipe xs p = stepPipe p >>= \case
--     PSDone r  -> pure ([], Right (xs, r))
--     PSWait f  -> case xs of
--       []   -> pure ([], Left (f . Right))
--       y:ys -> feedPipe ys (f (Right y))
--     PSOut o q -> first (o:) <$> feedPipe xs q

data PipeStep i o u m a =
      PSDone a
    | PSWait (Either u i -> Pipe i o u m a)
    | PSOut  o (Pipe i o u m a)
  deriving Functor

stepPipe
    :: Monad m
    => Pipe i o u m a
    -> m (PipeStep i o u m a)
stepPipe (Pipe p) = runFT p
    (pure . PSDone)
    (\pNext -> \case
        PAwaitF f g -> pure . PSWait  $ (unStep =<<) . lift . pNext . either f g
        PYieldF o x -> pure . PSOut o $ unStep =<< lift (pNext x)
    )

unStep :: PipeStep i o u m a -> Pipe i o u m a
unStep = \case
    PSDone a  -> pure a
    PSWait f  -> f =<< awaitEither
    PSOut o p -> yield o *> p

-- -- see feedPipe
-- squeezePipe
--     :: Monad m
--     => Pipe i o u m a
--     -> m ([o], Either (Either u i -> Pipe i o u m a) a)
-- squeezePipe p = stepPipe p >>= \case
--     PSDone a  -> pure ([], Right a)
--     PSWait f  -> pure ([], Left  f)
--     PSOut o q -> first (o:) <$> squeezePipe q

-- so now re-written to not use stepPipe and so not repeated runFT. it is
-- better than the stepPipe based version but still not quite as fast as
-- the full feedPipe for some reason. curious.
--
-- it might be just that recpipe is more performant.  but then why use FT
-- at all?  maybe FT is good for building but not for using
--
-- step-based: 1.7s
-- rec-based (feedpipe): 1.1s
-- single FT: 1.3s
squeezePipe
    :: Monad m
    => Pipe i o u m a
    -> m ([o], Either (Either u i -> Pipe i o u m a) a)
squeezePipe (Pipe p) = runFT p
    (pure . ([],) . Right)
    (\pNext -> \case
        PAwaitF f g -> pure . ([],) . Left $ (unSqueeze =<<) . lift . pNext . either f g
        PYieldF o x -> first (o:) <$> pNext x
    )
  where
    unSqueeze (os, next) = do
      mapM_ yield os
      case next of
        Left f  -> f =<< awaitEither
        Right a -> pure a
