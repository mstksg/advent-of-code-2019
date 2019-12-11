{-# LANGUAGE QuasiQuotes #-}

module AOC.Common.OCR (
    parseLetters
  ) where

import           AOC.Common
import           Control.Monad
import           Data.Bifunctor
import           Data.List
import           Data.Map       (Map)
import           Data.Semigroup
import           Data.Set       (Set)
import           Linear
import           Text.Heredoc   (here)
import qualified Data.Map       as M
import qualified Data.Set       as S

parseLetters
    :: Set Point
    -> String
parseLetters = map (\c -> M.findWithDefault '?' c letterMap) . unfoldr peel

-- | A map of a set of "on" points (for a 4x6 grid) to the letter they
-- represent
letterMap :: Map (Set Point) Char
letterMap = M.fromList
          . uncurry (zipWith (flip (,)))
          . second (unfoldr peel . M.keysSet . parseAsciiMap (guard . (== '#')))
          $ rawLetterforms

peel :: Set Point -> Maybe (Set Point, Set Point)
peel ps = do
    Min xMin <- flip foldMap ps $ \(V2 x _) -> Just (Min x)
    let ps' = subtract (V2 xMin 0) `S.map` ps
    pure $ S.partition (\(V2 x _) -> x < 4) ps'


-- | don't include y since it is a weird length...
rawLetterforms :: (String, String)
rawLetterforms = ("ABCEFGHJKLPRUZ", drop 1 [here|
.##.###..##.########.##.#..#..###..##...###.###.#..#####
#..##..##..##...#...#..##..#...##.#.#...#..##..##..#...#
#..####.#...###.###.#...####...###..#...#..##..##..#..#.
#####..##...#...#...#.###..#...##.#.#...###.###.#..#.#..
#..##..##..##...#...#..##..##..##.#.#...#...#.#.#..##...
#..####..##.#####....####..#.##.#..######...#..#.##.####
|])
