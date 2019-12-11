-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Common      (parseAsciiMap, countTrue)
import           AOC.Common.OCR
import           AOC.Common.OCR  (parseLetters)
import           AOC.Solver      ((:~>)(..), dyno_)
import           Control.Monad   (guard)
import           Data.List       (transpose)
import           Data.List.Split (chunksOf)
import           Data.Maybe      (listToMaybe)
import           Data.Ord        (comparing)
import           Safe            (minimumByMay)
import qualified Data.Map        as M

day08a :: String :~> Int
day08a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = fmap answer
             . minimumByMay (comparing (countTrue (== '0')))
             . chunksOf (dyno_ "w" 25 * dyno_ "h" 6)
    }
  where
    answer x  = countTrue (== '1') x * countTrue (== '2') x

day08b :: [String] :~> String
day08b = MkSol
    { sParse = Just . chunksOf 150
    -- , sShow  = unlines . chunksOf 25 . map (\case '0' -> ' '; _ -> '#')
    , sShow  = parseLetters
             . M.keysSet
             . parseAsciiMap (guard . (== '1'))
             . unlines
             . chunksOf 25
    , sSolve = traverse (listToMaybe . filter (/= '2')) . transpose
    }
