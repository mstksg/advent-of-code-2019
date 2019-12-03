{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Common         (Point, mannDist, Dir, dirPoint, parseDir)
import           AOC.Solver         ((:~>)(..))
import           Control.Monad      ((<=<))
import           Data.Foldable      (toList)
import           Data.List          (scanl')
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.List.Split    (splitOn)
import           Data.Map           (Map)
import           Text.Read          (readMaybe)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import qualified Data.Set           as S

type Path = [(Dir, Int)]

parsePath :: String -> Maybe Path
parsePath = traverse parsePoint . splitOn ","
  where
    parsePoint (d:ns) = (,) <$> parseDir d <*> readMaybe ns
    parsePoint _      = Nothing

-- | From a list of paths, get a Map of the points where they cross, along
-- with the minimum time sum to get to that point.
crossings :: NonEmpty Path -> Map Point Int
crossings = foldr1 (M.intersectionWith (+)) . fmap follow
  where
    -- a map of every point visted to the steps taken to visit it
    follow :: Path -> Map Point Int
    follow = M.fromListWith min
           . drop 1
           . flip zip [0..]
           . scanl' (+) 0
           . concatMap (uncurry expandDir)
    expandDir d ns = replicate ns (dirPoint d)

day03a :: NonEmpty Path :~> Int
day03a = MkSol
    { sParse = NE.nonEmpty <=< traverse parsePath . lines
    , sShow  = show
    , sSolve = S.lookupMin
             . S.map (mannDist 0)
             . M.keysSet
             . crossings
    }

day03b :: NonEmpty Path :~> Int
day03b = MkSol
    { sParse = NE.nonEmpty <=< traverse parsePath . lines
    , sShow  = show
    , sSolve = fmap minimum . NE.nonEmpty . toList . crossings
    }
