{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where

import           AOC.Solver ((:~>)(..))
import           Text.Read  (readMaybe)

fuel :: Int -> Int
fuel = subtract 2 . (`div` 3)

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = Just . sum . map fuel
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = Just . sum . map (sum . drop 1 . takeWhile (>= 0) . iterate fuel)
    }
