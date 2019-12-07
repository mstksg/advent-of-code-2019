{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day07 (
    day07a
  , day07b
  ) where

-- import           Data.Conduit
-- import           Data.Conduit.Lift
-- import qualified Data.Conduit.Combinators as C
import           AOC.Common.Intcode
import           AOC.Prelude
import           Control.Monad.Trans.Free
import           Control.Monad.Writer
import           Control.Monad.Except
import           Data.Conduino
import           Data.Conduino.Internal
import           Data.Sequence.NonEmpty      (NESeq)
import qualified Data.Conduino.Combinators   as C
import qualified Data.Map                    as M
import qualified Data.Sequence               as Seq
import qualified Data.Sequence.NonEmpty      as NESeq

-- data Memory = Mem
--     { mPos  :: Int
--     , mRegs :: Seq Int
--     }
--   deriving Show

-- data Mode = Pos | Imm
--   deriving (Eq, Ord, Enum, Show)

-- data Instr = Add | Mul | Get | Put | Jnz | Jez | Clt | Ceq | Hlt
--   deriving (Eq, Ord, Enum, Show)

-- instrMap :: Map Int Instr
-- instrMap = M.fromList $
--     (99, Hlt) : zip [1 ..] [Add ..]

-- instr :: Int -> Maybe Instr
-- instr = (`M.lookup` instrMap)

-- readMem
--     :: (MonadState Memory m, MonadError String m)
--     => m Int
-- readMem = do
--     Mem p r <- get
--     case Seq.lookup p r of
--       Nothing -> throwError "bad index"
--       Just x  -> x <$ put (Mem (p + 1) r)

-- peekMem
--     :: (MonadState Memory m, MonadError String m)
--     => Int -> m Int
-- peekMem i = do
--     Mem _ r <- get
--     case Seq.lookup i r of
--       Nothing -> throwError "bad index"
--       Just x  -> pure x

-- -- | Run a @t Int -> m r@ function by getting fetching an input container
-- -- @t Int@.
-- withInput
--     :: (Traversable t, Applicative t, MonadState Memory m, MonadError String m)
--     => Int      -- ^ mode int
--     -> (t Int -> m r)
--     -> m r
-- withInput mo f = do
--     inp <- for (fillModes mo) $ \mode -> do
--       a <- readMem
--       case mode of
--         Pos -> peekMem a
--         Imm  -> pure a
--     f inp

-- -- | Magically fills a fixed-shape 'Applicative' with each mode from a mode
-- -- op int.
-- fillModes :: (Traversable t, Applicative t) => Int -> t Mode
-- fillModes i = snd $ mapAccumL go i (pure ())
--   where
--     go j _ = (t, case o of 0 -> Pos; _ -> Imm)
--       where
--         (t,o) = j `divMod` 10

-- -- | Useful type to abstract over the actions of the different operations
-- data InstrRes = IRWrite Int         -- ^ write a value
--               | IRJump  Int         -- ^ jump to position
--               | IRNop               -- ^ do nothing
--               | IRHalt              -- ^ halt
--   deriving Show

-- step
--     :: (MonadError String m, MonadState Memory m)
--     => ConduitT Int Int m Bool
-- step = do
--     (mo, x) <- (`divMod` 100) <$> readMem
--     o  <- maybeToEither "bad instr" $ instr x
--     ir <- case o of
--       Add -> withInput mo $ \case V2 a b  -> pure . IRWrite $ a + b
--       Mul -> withInput mo $ \case V2 a b  -> pure . IRWrite $ a * b
--       Get -> await      >>= \case Nothing -> throwError "no input"
--                                   Just a  -> pure $ IRWrite a
--       Put -> withInput mo $ \case V1 a    -> IRNop <$ yield a
--       Jnz -> withInput mo $ \case V2 a b  -> pure $ if a /= 0 then IRJump b else IRNop
--       Jez -> withInput mo $ \case V2 a b  -> pure $ if a == 0 then IRJump b else IRNop
--       Clt -> withInput mo $ \case V2 a b  -> pure . IRWrite $ if a <  b then 1 else 0
--       Ceq -> withInput mo $ \case V2 a b  -> pure . IRWrite $ if a == b then 1 else 0
--       Hlt                                 -> pure IRHalt
--     case ir of
--       IRWrite y -> do
--         c <- readMem
--         True <$ modify (\(Mem p r) -> Mem p (Seq.update c y r))
--       IRJump  z ->
--         True <$ modify (\(Mem _ r) -> Mem z r)
--       IRNop     ->
--         pure True
--       IRHalt    ->
--         pure False

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

setupChain :: MonadError String m => Memory -> [Int] -> Pipe Int Int Void m Void
setupChain m = foldr ((.|) . prime) (C.map id)
  where
    prime i = (yield i *> C.map id)
           .| evalStateP m stepForeverAndDie

-- stepForeverAndDie
--     :: (MonadState Memory m, MonadError String m)
--     => Pipe Int Int Void m Void
-- stepForeverAndDie = untilFalse step *> throwError "no more input to give"

-- setupChain2 :: Memory -> [Int] -> Pipe Int Int Void Identity Void
-- setupChain2 m = foldr ((.|) . prime) (C.map id)
--   where
--     prime i = (yield i *> C.map id)
--            .| evalStateP m stepForeverAndDie


day07a :: _ :~> _
day07a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> maximumMay
            [ r
            | xs <- permutations [0..4]
            , Right r <- pure . runPipe $
                        (yield 0 *> throwError "no input")
                     .| setupChain m xs
                     .| awaitSurely
            ]
    }

day07b :: _ :~> _
day07b = MkSol
    { sParse = parseMem
    , sShow  = show
    -- , sSolve = Just
    , sSolve = \m -> maximumMay
            [ last r :: Int
            | xs <- permutations [5..9]
            , let r = runPipePure . runPipe $
                        void (runExceptP ((yield 0 *> throwError "no input") .| feedback (setupChain m xs)))
                     .| C.sinkList
            ]
    -- , sSolve = \m -> tailMay $ loopProg [9,8,7,6,5] m
    -- `9,8,7,6,5`):
    }

-- | loop a pipe into itself.
feedback :: (Monad m, Show x) => Pipe x x u m a -> Pipe x x u m a
feedback = fromRecPipe . feedback_ Seq.empty . toRecPipe

feedback_ :: (Monad m, Show x) => Seq x -> RecPipe x x u m a -> RecPipe x x u m a
feedback_ inp (FreeT p) = FreeT $ p >>= \case
    Pure x             -> pure $ Pure x
    Free (PAwaitF f g) -> case inp of
      Empty    -> pure $ Free (PAwaitF (feedback_ inp . f) (feedback_ inp . g))
      i :<| is -> runFreeT $ feedback_ is (g i)
    Free (PYieldF o q) -> pure $ Free $ PYieldF o (feedback_ (inp :|> o) q)



parseMem :: String -> Maybe Memory
parseMem = (=<<) (fmap (Mem 0 . NESeq.fromList) . nonEmpty)
         . traverse readMaybe
         . splitOn ","

-- loopProg :: [Int] -> Memory -> [Int]
-- loopProg  cs m0 = execWriter $
--     flip loopMaybeM (Just <$> cs, replicate 5 m0) $ \(cs', ms) -> do
--       let (ms', out) = runChain cs' ms
--       forM out $ \o -> do
--         tell [o]
--         pure (Nothing <$ cs', ms')


-- runChain :: [Maybe Int] -> [Memory] -> ([Memory], Maybe Int)
-- runChain (traceShowId->[x1,x2,x3,x4,x5]) [n1,n2,n3,n4,n5] = either error id $ go
--   where
--     go :: Either String ([Memory], Maybe Int)
--     go = fmap (first shuffle) . runConduit $
--          (mapM_ yield x1 *> yield 0)
--       .| (mapM_ yield x2 *> execStateC n1 (untilFalse step))
--          `fuseBoth` (mapM_ yield x3 *> execStateC n2 (untilFalse step))
--          `fuseBoth` (mapM_ yield x4 *> execStateC n3 (untilFalse step))
--          `fuseBoth` (mapM_ yield x5 *> execStateC n4 (untilFalse step))
--          `fuseBoth` execStateC n5 (untilFalse step)
--          `fuseBoth` C.head
--     shuffle ((((m1,m2),m3),m4),m5) = [m1,m2,m3,m4,m5]

-- runProg' :: [Int] -> Memory -> [Int]
-- runProg' [x1,x2,x3,x4,x5] m = go
--   where
--     go :: [Int]
--     go = runConduitPure $
--          (yield x1 *> yield 0 *> C.yieldMany go)
--       .| (yield x2 *> void (runExceptC $ runStateC m (untilFalse step)))
--       .| (yield x3 *> void (runExceptC $ runStateC m (untilFalse step)))
--       .| (yield x4 *> void (runExceptC $ runStateC m (untilFalse step)))
--       .| (yield x5 *> void (runExceptC $ runStateC m (untilFalse step)))
--       .| void (runExceptC $ runStateC m (untilFalse step))
--       .| C.sinkList

