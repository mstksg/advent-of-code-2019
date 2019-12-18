-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day16 (
    day16a
  , day16b
  , binom99
  ) where

import           AOC.Common                   ((!!!), digitToIntSafe)
import           AOC.Solver                   ((:~>)(..))
import           Control.Monad
import           Control.Monad.ST             (runST)
import           Control.Monad.State          (evalStateT, get, put)
import           Data.Foldable                (forM_)
import           Data.List                    (tails, unfoldr)
import           Data.Maybe                   (mapMaybe)
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as MVS

day16a :: [Int] :~> [Int]
day16a = MkSol
    { sParse = Just . mapMaybe digitToIntSafe
    , sShow  = concatMap show
    , sSolve = Just
             . VS.toList
             . VS.take 8
             . (!!! 100)
             . iterate stepVec
             . VS.fromList
    }


day16b :: [Int] :~> [Int]
day16b = MkSol
    { sParse = Just . mapMaybe digitToIntSafe
    , sShow  = concatMap show
    , sSolve = \str ->
        let origLen    = length str
            n          = read . concatMap show $ take 7 str
            startPoint = n `mod` origLen
            endPoint   = origLen * 10000 - n
            xs         = take endPoint . drop startPoint . cycle $ str
            result     = map (`dot` binom99) (tails xs)
            good       = n >= (origLen * 5000)

        in  take 8 result <$ guard good
    }
  where
    dot xs ys = (`mod` 10) . sum . map (`mod` 10) $ zipWith (*) xs ys

-- | Binomial(n+99,99)
binom99 :: [Int]
binom99 = fromIntegral . (`mod` 10) <$> unfoldr go (99, fac99)
  where
    fac99 :: Integer
    fac99 = product [1..99]
    go (id->(!n, !nfac)) = Just (x, (n', nfac'))
      where
        x     = nfac `div` fac99
        n'    = n + 1
        nfac' = (nfac `div` (n' - 99)) * n'


-- | needlessly over-optimized
stepVec :: VS.Vector Int -> VS.Vector Int
stepVec v = runST $ do
    mv <- MVS.replicate (VS.length v) 0
    flip evalStateT (0,[]) . flip VS.mapM_ v $ \x -> do
      (i, steps0) <- get
      let !i'    = i + 1
          !steps = newStep (i + 1) : map succStep steps0
      put (i', steps)
      forM_ (zip [0..] steps) $ \(j, s) ->
        forM_ (stepOut s) $ \q ->
          MVS.modify mv ((q * x) +) (i - j)
    VS.map ((`mod` 10) . abs) <$> VS.freeze mv

data Step = Step { sSize :: !Int, sPhase :: !Int }
  deriving Show

stepOut :: Step -> Maybe Int
stepOut Step{..} = case (sPhase `div` sSize) `mod` 4 of
    0 -> Just 1
    2 -> Just (-1)
    _ -> Nothing

succStep :: Step -> Step
succStep Step{..} = Step sSize (sPhase + 1)

newStep :: Int -> Step
newStep n = Step n 0
