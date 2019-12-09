{-# LANGUAGE QuasiQuotes #-}

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

import           AOC.Common      (Point, parseAsciiMap, countTrue)
import           AOC.Solver      ((:~>)(..), dyno_)
import           Control.Monad   (guard)
import           Data.Bifunctor  (second)
import           Data.List       (unfoldr, transpose)
import           Data.List.Split (chunksOf)
import           Data.Map        (Map)
import           Data.Maybe      (listToMaybe, mapMaybe)
import           Data.Ord        (comparing)
import           Data.Semigroup  (Min(..))
import           Data.Set        (Set)
import           Linear          (V2(..))
import           Safe            (minimumByMay)
import           Text.Heredoc    (here)
import qualified Data.Map        as M
import qualified Data.Set        as S

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
    , sShow  = mapMaybe (`M.lookup` letterMap)
             . unfoldr peel
             . M.keysSet
             . parseAsciiMap (guard . (== '1'))
             . unlines
             . chunksOf 25
    , sSolve = traverse (listToMaybe . filter (/= '2')) . transpose
    }

peel :: Set Point -> Maybe (Set Point, Set Point)
peel ps = do
    Min xMin <- flip foldMap ps $ \(V2 x _) -> Just (Min x)
    let ps' = subtract (V2 xMin 0) `S.map` ps
    pure $ S.partition (\(V2 x _) -> x < 4) ps'

-- | A map of a set of "on" points (for a 4x6 grid) to the letter they
-- represent
letterMap :: Map (Set Point) Char
letterMap = M.fromList
          . uncurry (zipWith (flip (,)))
          . second (unfoldr peel . M.keysSet . parseAsciiMap (guard . (== '#')))
          $ rawLetterforms

rawLetterforms :: (String, String)
rawLetterforms = ("ABCEFGHJKLPRUYZ", drop 1 [here|
.##.###..##.########.##.#..#..###..##...###.###.#..##...#####
#..##..##..##...#...#..##..#...##.#.#...#..##..##..##...#...#
#..####.#...###.###.#...####...###..#...#..##..##..#.#.#...#.
#####..##...#...#...#.###..#...##.#.#...###.###.#..#..#...#..
#..##..##..##...#...#..##..##..##.#.#...#...#.#.#..#..#..#...
#..####..##.#####....####..#.##.#..######...#..#.##...#..####
|])
