-- |
-- Module      : AOC.Challenge.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day24 (
    day24a
  , day24b
  ) where

import           AOC.Common      (Point, cardinalNeighbsSet, parseAsciiMap, firstRepeated, (!!!), Dir(..))
import           AOC.Solver      ((:~>)(..), dyno_)
import           Control.DeepSeq (NFData)
import           Data.Finite     (Finite, finites)
import           Data.Semigroup  (Min(..), Max(..), Sum(..))
import           Data.Set        (Set)
import           GHC.Generics    (Generic)
import           Linear.V2       (V2(..))
import qualified Data.Map        as M
import qualified Data.Set        as S

allPoints :: Set Point
allPoints = S.fromList $ V2 <$> [0..4] <*> [0..4]

step1 :: Set Point -> Set Point
step1 s0 = flip S.filter allPoints $ \p ->
    let n = S.size $ cardinalNeighbsSet p `S.intersection` s0
    in  if p `S.member` s0
          then n == 1
          else n == 1 || n == 2

day24a :: Set Point :~> Set Point
day24a = MkSol
    { sParse = Just . parseMap
    , sShow  = show . getSum . foldMap (Sum . biodiversity)
    , sSolve = firstRepeated . iterate step1
    }
  where
    biodiversity :: Point -> Int
    biodiversity (V2 x y) = 2 ^ i
      where
        i = y * 5 + x

-- | Position in layer.  Cannot be (2,2).  Use 'mkP5' if you're not sure.
type P5 = V2 (Finite 5)

-- | Safely construct a 'P5' that is not (2,2)
mkP5 :: Finite 5 -> Finite 5 -> Maybe P5
mkP5 2 2 = Nothing
mkP5 x y = Just (V2 x y)

data Loc = L
    { lLevel :: Int         -- ^ positive: zoom in, negative: zoom out
    , lPoint :: P5          -- ^ position in layer.
    }
  deriving (Eq, Ord, Show, Generic)
instance NFData Loc

stepLoc :: Loc -> Dir -> Set Loc
stepLoc (L n p@(V2 x y)) = fmap S.fromList $ \case
    North -> case p of
      V2 _ 0 -> [L (n - 1) (V2 2 1)]
      V2 2 3 -> L (n + 1) . (`V2` 4) <$> finites
      _      -> [L n (V2 x (y - 1))]
    East -> case p of
      V2 4 _ -> [L (n - 1) (V2 3 2)]
      V2 1 2 -> L (n + 1) . V2 0 <$> finites
      _      -> [L n (V2 (x + 1) y)]
    South -> case p of
      V2 _ 4 -> [L (n - 1) (V2 2 3)]
      V2 2 1 -> L (n + 1) . (`V2` 0) <$> finites
      _      -> [L n (V2 x (y + 1))]
    West  -> case p of
      V2 0 _ -> [L (n - 1) (V2 1 2)]
      V2 3 2 -> L (n + 1) . V2 4 <$> finites
      _      -> [L n (V2 (x - 1) y)]

step2 :: Set Loc -> Set Loc
step2 s0 = flip S.filter (oldLocs <> zoomOut) $ \p ->
    let n = S.size $ neighbs p `S.intersection` s0
    in  if p `S.member` s0
          then n == 1
          else n == 1 || n == 2
  where
    neighbs p = foldMap (stepLoc p) [North ..]
    oldLocs = S.fromList
        [ L n p
        | n      <- [mn .. mx + 1]
        , Just p <- mkP5 <$> finites <*> finites
        ]
    -- a little optimization: only check the center 9 points in the zoomed
    -- out layer
    zoomOut = S.fromList
        [ L (mn - 1) p
        | Just p <- mkP5 <$> [1..3] <*> [1..3]
        ]
    (Min mn, Max mx) = foldMap (\(lLevel->l) -> (Min l, Max l)) . S.toList $ s0


day24b :: Set Loc :~> Set Loc
day24b = MkSol
    { sParse = Just . S.map (L 0 . fmap fromIntegral) . parseMap
    , sShow  = show . S.size
    , sSolve = Just . (!!! dyno_ "steps" 200) . iterate step2
    }

parseMap :: String -> Set Point
parseMap = M.keysSet . M.filter (== '#') . parseAsciiMap Just
