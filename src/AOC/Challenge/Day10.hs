-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day10 (
    day10a
  , day10b
  ) where

import           AOC.Common              (Point, parseAsciiMap)
import           AOC.Solver              ((:~>)(..))
import           Control.Monad           (guard)
import           Data.Fixed              (mod')
import           Data.Foldable           (toList)
import           Data.List               (sortOn, maximumBy, unfoldr)
import           Data.Maybe              (listToMaybe)
import           Data.Ord                (comparing)
import           Data.Semigroup          (Max(..))
import           Data.Semigroup.Foldable (foldMap1)
import           Data.Set.NonEmpty       (NESet)
import           Linear                  (V2(..))
import           Linear.Vector           ((*^))
import qualified Data.Map                as M
import qualified Data.Set.NonEmpty       as NES

lineTo :: Point -> Point -> [Point]
lineTo p0 p1
    | dx == 0   = V2 minX     <$> [minY + 1 .. maxY - 1]
    | dy == 0   = (`V2` minY) <$> [minX + 1 .. maxX - 1]
    | gcf > 1   = [ p0 + n *^ step | n <- [1 .. gcf - 1] ]
    | otherwise = []
  where
    V2 minX minY = min <$> p0 <*> p1
    V2 maxX maxY = max <$> p0 <*> p1
    d@(V2 dx dy) = p1 - p0
    gcf          = gcd dx dy
    step         = (`div` gcf) <$> d

angleTo :: Point -> Point -> Double
angleTo p0 p1 = (-atan2 (fromIntegral dx) (fromIntegral dy) + pi) `mod'` (2 * pi)
  where
    V2 dx dy = p1 - p0

viewable :: NESet Point -> Point -> [Point]
viewable s p = filter good . toList . NES.delete p $ s
  where
    good q = all (`NES.notMember` s) (lineTo p q)

day10a :: NESet Point :~> Int
day10a = MkSol
    { sParse = NES.nonEmptySet . M.keysSet . parseAsciiMap (\c -> guard (c == '#'))
    , sShow  = show
    , sSolve = \s -> Just
                   . getMax
                   . foldMap1 (Max . length . viewable s)
                   $ s
    }

day10b :: NESet Point :~> Point
day10b = MkSol
    { sParse = NES.nonEmptySet . M.keysSet . parseAsciiMap (\c -> guard (c == '#'))
    , sShow  = \case V2 x y -> show $ x * 100 + y
    , sSolve = \s ->
        let station = maximumBy (comparing (length . viewable s)) s
            s'      = NES.delete station s
        in  listToMaybe . drop 199 $ unfoldr (shootFrom station) (Nothing, s')
    }
  where
    shootFrom p (aim, s) = do
        s' <- NES.nonEmptySet s
        let targ:next:_ = dropper . cycle . sortOn (angleTo p) $ viewable s' p
        pure (targ, (Just next, NES.delete targ s'))
      where
        dropper = case aim of
          Nothing -> id
          Just a  -> dropWhile (/= a)

