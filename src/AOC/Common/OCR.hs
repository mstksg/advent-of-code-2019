{-# LANGUAGE QuasiQuotes #-}

module AOC.Common.OCR (
    parseLetters
  , contiguousShapes
  ) where

import           AOC.Common
import           Control.Lens
import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable
import           Data.List
import           Data.Map          (Map)
import           Data.Profunctor
import           Data.Semigroup
import           Data.Set          (Set)
import           Data.Set.NonEmpty (NESet)
import           Linear
import           Text.Heredoc      (here)
import qualified Control.Foldl     as F
import qualified Data.Map          as M
import qualified Data.Set          as S
import qualified Data.Set.NonEmpty as NES

-- | The set of unconnected shapes, indexed by their original center of
-- mass
contiguousShapes :: Set Point -> Map (V2 Double) (NESet Point)
contiguousShapes s0 = M.fromList
    [ (com, NES.map (subtract topCorner) s)
    | NES.IsNonEmpty s <- S.toList . S.map flood $ s0
    , let com            = F.fold ((lmap . fmap) fromIntegral F.mean) s
          V2 topCorner _ = boundingBox s
    ]
  where
    flood = floodFill (S.fromList . filter (`S.member` s0) . fullNeighbs)
          . S.singleton

-- | The set of unconnected shapes, sorted against some function on their
-- original center of masses
contiguousShapesBy
    :: Ord a
    => (V2 Double -> a)
    -> Set Point
    -> [NESet Point]
contiguousShapesBy f = toList . M.mapKeys f . contiguousShapes

parseLetters
    :: Set Point
    -> String
parseLetters = map (\c -> M.findWithDefault '?' c letterMap)
             . toList
             . contiguousShapesBy (view _x)

-- | A map of a set of "on" points (for a 4x6 grid) to the letter they
-- represent
letterMap :: Map (NESet Point) Char
letterMap = M.fromList
          . uncurry (zipWith (flip (,)))
          . second ( contiguousShapesBy (view _x)
                   . M.keysSet
                   . parseAsciiMap (guard . (== '#'))
                   )
          $ rawLetterforms

rawLetterforms :: (String, String)
rawLetterforms = ("ABCEFGHJKLPRUYZ", drop 1 [here|
.##..###...##..####.####..##..#..#...##.#..#.#....###..###..#..#.#...#.####
#..#.#..#.#..#.#....#....#..#.#..#....#.#.#..#....#..#.#..#.#..#.#...#....#
#..#.###..#....###..###..#....####....#.##...#....#..#.#..#.#..#..#.#....#.
####.#..#.#....#....#....#.##.#..#....#.#.#..#....###..###..#..#...#....#..
#..#.#..#.#..#.#....#....#..#.#..#.#..#.#.#..#....#....#.#..#..#...#...#...
#..#.###...##..####.#.....###.#..#..##..#..#.####.#....#..#..##....#...####
|])
