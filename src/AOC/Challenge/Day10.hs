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

import           AOC.Common              (Point, parseAsciiMap, maximumValNE, lineTo, drop')
import           AOC.Solver              ((:~>)(..))
import           Control.Monad           (guard)
import           Data.Foldable           (toList)
import           Data.List               (sortOn, unfoldr)
import           Data.Maybe              (listToMaybe)
import           Data.Semigroup          (Max(..))
import           Data.Semigroup.Foldable (foldMap1)
import           Data.Set.NonEmpty       (NESet)
import           Linear                  (V2(..))
import qualified Data.Map                as M
import qualified Data.Map.NonEmpty       as NEM
import qualified Data.Set.NonEmpty       as NES

angleTo :: Point -> Point -> Double
angleTo p0 p1 = atan2 (-fromIntegral dx) (fromIntegral dy)
  where
    V2 dx dy = p1 - p0

viewableIn :: NESet Point -> Point -> [Point]
viewableIn s p = filter good . toList . NES.delete p $ s
  where
    good q = all (`NES.notMember` s) (lineTo p q)

day10a :: NESet Point :~> Int
day10a = MkSol
    { sParse = NES.nonEmptySet . M.keysSet . parseAsciiMap (\c -> guard (c == '#'))
    , sShow  = show
    , sSolve = \as -> Just . getMax . foldMap1 (Max . length . viewableIn as) $ as
    }

day10b :: NESet Point :~> Point
day10b = MkSol
    { sParse = NES.nonEmptySet . M.keysSet . parseAsciiMap (\c -> guard (c == '#'))
    , sShow  = \case V2 x y -> show $ x * 100 + y
    , sSolve = \as ->
        let (station, _) = maximumValNE $ NEM.fromSet (length . viewableIn as) as
            as'          = NES.delete station as
        in  listToMaybe . drop' 199 $
              unfoldr (uncurry (shootFrom station)) (Nothing, as')
    }
  where
    shootFrom p aim as = do
        as' <- NES.nonEmptySet as
        let targ:next:_ = dropper . cycle . sortOn (angleTo p) $ viewableIn as' p
        pure (targ, (Just next, NES.delete targ as'))
      where
        dropper = case aim of
          Nothing -> id
          Just a  -> dropWhile (/= a)

