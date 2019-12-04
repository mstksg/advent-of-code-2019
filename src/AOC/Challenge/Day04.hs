-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import           AOC.Solver      ((:~>)(..))
import           Data.Char       (digitToInt)
import           Data.List       (group)
import           Data.List.Split (splitOn)

range :: String -> Maybe [Int]
range (map read . splitOn "-"->[x,y]) = Just [x..y]
range _                               = Nothing

digits :: Int -> [Int]
digits = map digitToInt . show

consecs :: [a] -> [(a,a)]
consecs xs = zip xs (drop 1 xs)

monotonic :: Ord a => [a] -> Bool
monotonic = all (\(x,y) -> y >= x) . consecs

doubles :: Eq a => [a] -> Bool
doubles = any (uncurry (==)) . consecs

strictDoubles :: Eq a => [a] -> Bool
strictDoubles = any ((== 2) . length) . group

day04a :: [Int] :~> Int
day04a = MkSol
    { sParse = range
    , sShow  = show
    , sSolve = Just
             . length
             . filter (\x -> all ($ digits x) [monotonic, doubles])
    }

day04b :: [Int] :~> Int
day04b = MkSol
    { sParse = range
    , sShow  = show
    , sSolve = Just
             . length
             . filter (\x -> all ($ digits x) [monotonic, strictDoubles])
    }
