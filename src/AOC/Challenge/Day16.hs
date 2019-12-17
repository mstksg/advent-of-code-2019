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
  ) where

import           AOC.Common          ((!!!), digitToIntSafe)
import           AOC.Solver          ((:~>)(..))
import           Control.Monad.ST    (runST)
import           Control.Monad.State (evalStateT, modify, get)
import           Data.Foldable       (forM_)
import           Data.Maybe          (mapMaybe)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

day16a :: [Int] :~> [Int]
day16a = MkSol
    { sParse = Just . mapMaybe digitToIntSafe
    , sShow  = concatMap show
    , sSolve = Just
             . V.toList
             . V.take 8
             . (!!! 100)
             . iterate stepVec
             . V.fromList
    }


day16b :: [Int] :~> [Int]
day16b = MkSol
    { sParse = Just . mapMaybe digitToIntSafe
    , sShow  = concatMap show
    , sSolve = \str -> Just $
        let origLen = length str
            n   = read . concatMap show $ take 7 str    -- we hope this number is bigger than half length str
            startPoint = n `mod` origLen
            endPoint   = origLen * 10000 - n
            xs  = take endPoint . drop startPoint . cycle $ str
        in  take 8
              . reverse
              . (!!! 100)
              . iterate ezStep
              . reverse
              $ xs
    }

-- | ez pz
ezStep :: [Int] -> [Int]
ezStep = map (`mod` 10) . scanl1 (+)

-- | needlessly over-optimized
stepVec :: V.Vector Int -> V.Vector Int
stepVec v = runST $ do
    mv <- MV.replicate (V.length v) 0
    flip evalStateT [] . flip V.imapM_ v $ \i x -> do
      modify $ (newStep (i + 1) :) . map succStep
      steps <- get
      forM_ (zip [0..] steps) $ \(j, s) ->
        forM_ (stepOut s) $ \q ->
          MV.modify mv ((q * x) +) (i - j)
    fmap ((`mod` 10) . abs) <$> V.freeze mv

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
