{-# LANGUAGE TypeApplications         #-}
{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day19 (
    day19a
  , day19b
  ) where

import           AOC.Common.Conduino
import           AOC.Common.Intcode
import           AOC.Common.Search
import           AOC.Prelude
import           Data.Conduino
import           Linear.V2
import qualified Data.Conduino.Combinators as C
import qualified Data.Map                  as M
import qualified Data.Set                  as S

day19a :: _ :~> _
day19a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> Just $ countTrue (gobo m) (V2 <$> [0..49] <*> [0..49])
    }

gobo :: Memory -> Point -> Bool
gobo m (V2 x y) = (== 1) . fromJust . join $
    runPipe $ (yield x *> yield y *> empty)
           .| untilHalt (stepForever @IErr m)
           .| await

data Ranges = R
    { xMins :: Map Int (Maybe Int)
    , xMaxs :: Map Int (Maybe Int)
    , yMins :: Map Int (Maybe Int)
    , yMaxs :: Map Int (Maybe Int)
    }

mkRanges :: Map Point Bool -> Ranges
mkRanges cache = R{..}
  where
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

day19b :: _ :~> _
day19b = MkSol
    { sParse = parseMem
    , sShow  = \(x,y) -> show $ x * 10000 + y
    , sSolve = \m -> do
        let cache   = M.fromSet (gobo m) (S.fromList $ V2 <$> [0..1500] <*> [0..1500])
            R{..}   = mkRanges cache
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
