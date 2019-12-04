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
import           Data.List       (group)
import           Data.List.Split (splitOn)
import           Text.Read       (readMaybe)

range :: String -> Maybe [Int]
range str = do
    [x,y] <- traverse readMaybe . splitOn "-" $ str
    pure [x..y]

consecs :: [a] -> [(a,a)]
consecs xs = zip xs (drop 1 xs)

monotonic :: Ord a => [a] -> Bool
monotonic = all (\(x,y) -> y >= x) . consecs

doubles :: Eq a => [a] -> Bool
doubles = any (uncurry (==)) . consecs

strictDoubles :: Eq a => [a] -> Bool
strictDoubles = any ((== 2) . length) . group

-- NOTE TO SELF: next time just literally type in the numbers here, heh, no
-- need to parse
day04a :: [Int] :~> Int
day04a = MkSol
    { sParse = range
    , sShow  = show
    , sSolve = Just
             . length
             . filter (\x -> all ($ show x) [monotonic, doubles])
    }

day04b :: [Int] :~> Int
day04b = MkSol
    { sParse = range
    , sShow  = show
    , sSolve = Just
             . length
             . filter (\x -> all ($ show x) [monotonic, strictDoubles])
    }
