{-# LANGUAGE TypeApplications         #-}

-- |
-- Module      : AOC.Challenge.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day19 (
    day19a
  , day19b
  ) where

import           AOC.Common          (Point, countTrue)
import           AOC.Common.Intcode  (Memory, IErr, parseMem, stepForever, untilHalt)
import           AOC.Common.Search   (binaryMinSearch, binaryFindMin)
import           AOC.Solver          ((:~>)(..))
import           AOC.Util            (firstJust)
import           Control.Applicative (empty)
import           Control.Lens        (view)
import           Control.Monad       (guard, join)
import           Data.Conduino       (runPipe, (.|), yield, await)
import           Data.List           (find)
import           Data.Map            (Map)
import           Data.Maybe          (fromJust)
import           Linear.V2           (V2(..), _x, _y)
import qualified Data.Map            as M
import qualified Data.Set            as S

day19a :: Memory :~> Int
day19a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> Just $ countTrue (checkBeam m) (V2 <$> [0..49] <*> [0..49])
    }

checkBeam :: Memory -> Point -> Bool
checkBeam m (V2 x y) = (== 1) . fromJust . join $
    runPipe $ (yield x *> yield y *> empty)
           .| untilHalt (stepForever @IErr m)
           .| await

data Ranges = R
    { xMins :: !(Map Int (Maybe Int))
    , xMaxs :: !(Map Int (Maybe Int))
    , yMins :: !(Map Int (Maybe Int))
    , yMaxs :: !(Map Int (Maybe Int))
    }

mkRanges :: Memory -> Ranges
mkRanges m = R{..}
  where
    cache   = M.fromSet (checkBeam m) (S.fromList $ V2 <$> [0..1500] <*> [0..1500])
    rangeRange = S.fromAscList [0..1250]
    xMins = flip M.fromSet rangeRange $ \y ->
      case M.lookup (y - 1) xMins of
        Just (Just xm) -> view _x <$> find (cache M.!) ((`V2` y) . (+ xm) <$> [0..10])
        _              -> view _x <$> find (cache M.!) ((`V2` y)          <$> [0..10])
    xMaxs = flip M.fromSet rangeRange $ \y ->
      case M.lookup y xMins of
        Just (Just xm) -> subtract 1 <$> binaryMinSearch (not . (cache M.!) . (`V2` y)) xm (xm + 250)
        _              -> Nothing
    yMins = flip M.fromSet rangeRange $ \x ->
      case M.lookup (x - 1) yMins of
        Just (Just ym) -> view _y <$> find (cache M.!) (V2 x . (+ ym) <$> [0..10])
        _              -> view _y <$> find (cache M.!) (V2 x          <$> [0..10])
    yMaxs = flip M.fromSet rangeRange $ \x ->
      case M.lookup x yMins of
        Just (Just ym) -> subtract 1 <$> binaryMinSearch (not . (cache M.!) . V2 x) ym (ym + 250)
        _              -> Nothing

day19b :: Memory :~> (Int, Int)
day19b = MkSol
    { sParse = parseMem
    , sShow  = \(x,y) -> show $ x * 10000 + y
    , sSolve = \m -> do
        let R{..}   = mkRanges m
            goodY y = do
                guard $ (xmax - xmin + 1) >= 100
                binaryFindMin goodX xmin (xmax - 100 + 1)
              where
                Just xmin = xMins M.! y
                Just xmax = xMaxs M.! y
                goodX x = do
                  let Just ymin = yMins M.! x
                      Just ymax = yMaxs M.! x
                  guard $ y >= ymin
                  guard $ (ymax - y + 1) >= 100
                  pure (x, y)
        (_,y) <- binaryFindMin goodY 500 1500
                -- this has a hole, how weird
                -- works for 825, 828, 829, 830...
                -- so it will get a false match on 828
        -- check a couple lower ys to be safe
        firstJust goodY [y-4 .. y]
    }
