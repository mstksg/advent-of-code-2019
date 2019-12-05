{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Prelude
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Containers.NonEmpty (onNonEmpty)
import qualified Data.Sequence.NonEmpty   as NESeq


data Memory = Mem
    { mPos  :: Int
    , mRegs :: NESeq Int
    }
  deriving Show

data Mode = Po | Im

instr :: Int -> Int
instr = (`mod` 100)

mode1 :: Int -> Mode
mode1 = toMode . (`mod` 10) . (`div` 100)

mode2 :: Int -> Mode
mode2 = toMode . (`div` 1000)

toMode 0 = Po
toMode 1 = Im

-- m2m :: Alternative m => Maybe a -> m a
-- m2m = maybe empty pure

m2m :: Applicative m => Maybe a -> m a
m2m = maybe undefined pure

step :: (MonadWriter [Int] m, MonadState [Int] m) => Memory -> m (Maybe Memory)
step (Mem (p) r) = do
    x <- m2m $ NESeq.lookup p r
    case instr x of
      1 -> do
        V3 a b c  <- m2m $ traverse (`NESeq.lookup` r) (V3 p p p + V3 1 2 3)
        y <- case mode1 x of
          Po -> m2m $ NESeq.lookup a r
          Im -> pure a
        z <- case mode2 x of
          Po -> m2m $ NESeq.lookup b r
          Im -> pure b
        pure . Just $ Mem (p + 4) (NESeq.update c (y + z) r)
      2 -> do
        V3 a b c  <- m2m $ traverse (`NESeq.lookup` r) (V3 p p p + V3 1 2 3)
        y <- case mode1 x of
          Po -> m2m $ NESeq.lookup a r
          Im -> pure a
        z <- case mode2 x of
          Po -> m2m $ NESeq.lookup b r
          Im -> pure b
        pure . Just $ Mem (p + 4) (NESeq.update c (y * z) r)
      3 -> do
        c <- m2m $ NESeq.lookup (p + 1) r
        y <- state $ \case i:is -> (i, is)
        pure . Just $ Mem (p + 2) (NESeq.update c y r)
      4 -> do
        c <- m2m $ NESeq.lookup (p + 1) r
        y <- m2m $ NESeq.lookup c r
        tell [y]
        pure . Just $ Mem (p + 2) r
      5 -> do
        V2 a b <- m2m $ traverse (`NESeq.lookup` r) (V2 p p + V2 1 2)
        y <- case mode1 x of
          Po -> m2m $ NESeq.lookup a r
          Im -> pure a
        z <- case mode2 x of
          Po -> m2m $ NESeq.lookup b r
          Im -> pure b
        let p' | y /= 0    = z
               | otherwise = p + 3
        pure . Just $ Mem p' r
      6 -> do
        V2 a b <- m2m $ traverse (`NESeq.lookup` r) (V2 p p + V2 1 2)
        y <- case mode1 x of
          Po -> m2m $ NESeq.lookup a r
          Im -> pure a
        z <- case mode2 x of
          Po -> m2m $ NESeq.lookup b r
          Im -> pure b
        let p' | y == 0    = z
               | otherwise = p + 3
        pure . Just $ Mem p' r
      7 -> do
        V3 a b c  <- m2m $ traverse (`NESeq.lookup` r) (V3 p p p + V3 1 2 3)
        y <- case mode1 x of
          Po -> m2m $ NESeq.lookup a r
          Im -> pure a
        z <- case mode2 x of
          Po -> m2m $ NESeq.lookup b r
          Im -> pure b
        let q | y < z    = 1
              | otherwise = 0
        pure . Just $ Mem (p + 4) (NESeq.update c q r)
      8 -> do
        V3 a b c  <- m2m $ traverse (`NESeq.lookup` r) (V3 p p p + V3 1 2 3)
        y <- case mode1 x of
          Po -> m2m $ NESeq.lookup a r
          Im -> pure a
        z <- case mode2 x of
          Po -> m2m $ NESeq.lookup b r
          Im -> pure b
        let q | y == z   = 1
              | otherwise = 0
        pure . Just $ Mem (p + 4) (NESeq.update c q r)
      99 -> pure Nothing
      _ -> error "why"

runProg :: [Int] -> Memory -> [Int]
runProg inp m = execWriter $ evalStateT (loopMaybeM step m) inp
        -- execWriter (evalStateT (loopMaybeM step m) [1])
  -- where
  --   go = go <=< step
  --       NESeq.head . mRegs . loopMaybe step

-- | Apply function until 'Nothing' is produced, and return last produced
-- value.
loopMaybeM
    :: Monad m => (a -> m (Maybe a))
    -> a
    -> m a
loopMaybeM f = go
  where
    go !x = f x >>= \case
      Nothing -> pure x
      Just !y -> go y

day05a :: _ :~> _
day05a = MkSol
    { sParse = parseMem
    , sShow  = show . last
    , sSolve = Just . runProg [1]
    }

day05b :: _ :~> _
day05b = MkSol
    { sParse = parseMem
    , sShow  = show . last
    , sSolve = Just . runProg [5]
    }

parseMem :: String -> Maybe Memory
parseMem = (onNonEmpty (Mem 0 . NESeq.fromList) =<<)
         . traverse readMaybe
         . splitOn ","

