{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day12 (
    day12a
  , day12b
  ) where

import           AOC.Prelude
import           Data.Maybe
import           Linear hiding     (transpose)
import qualified Data.Set          as S
import qualified Data.Vector.Sized as SV

-- <x=-8, y=-18, z=6>
-- <x=-11, y=-14, z=4>
-- <x=8, y=-3, z=-10>
-- <x=-2, y=-16, z=1>

type Phase = V2 (V3 Int)

parseVel = go . map read . words . clearOut p
  where
    go [x,y,z] = V2 (V3 x y z) 0
    p '-' = False
    p x   = not $ isDigit x

-- clearOut :: (Char -> Bool) -> String -> String
-- clearOut p = map $ \c -> if p c then ' '
--                                 else c

getAccels :: V4 Phase -> V4 (V3 Int)
getAccels xs = fmap acc xs
  where
    acc (V2 x _) = getSum . foldMap (Sum . signum . subtract x . view _x) $ xs

day12a :: V4 Phase :~> Int
day12a = MkSol
    { sParse = Just . (\[x,y,z,a] -> V4 x y z a) . map parseVel . lines
    , sShow  = show
    , sSolve = Just . sum . fmap energy . (!!! 1000) . iterate go
    }
  where
    go ps = ps''
      where
        a   = getAccels ps
        ps' = (\(V2 x v) a -> V2 x (v + a)) <$> ps <*> a
        ps'' = fmap (\(V2 x v) -> V2 (x + v) v) ps'
    energy (V2 x v) = mannDist x 0 * mannDist v 0
        

day12b :: V4 Phase :~> _
day12b = MkSol
    { sParse = Just . (\[x,y,z,a] -> V4 x y z a) . map parseVel . lines
    , sShow  = show
    -- , sSolve = countFirstRepeat . map (S.fromList . toList . fmap (view _x)) . iterate go
    -- , sSolve = countFirstRepeat . map (fmap (view _x)) . iterate go
    -- , sSolve = Just . countFirstRepeat . map (view _x . fmap (view (_x . _x))) . iterate go
    -- , sSolve = Just . countFirstRepeat . (!! 3) . transpose . map (foldMap toList . fmap (view _x)) . iterate go
    -- , sSolve = Just . fmap countFirstRepeat . sequenceA . fromJust
    -- , sSolve = Just . foldl1 lcm . fmap (fromJust . countFirstRepeat) . (\xs -> SV.generate (\i -> map (`SV.index` i) xs))
    -- , sSolve = Just . countFirstCycle . map (view _y . fmap (view (_x . _x))) . iterate go
    -- , sSolve = Just . take 100 . map (view _y . fmap (view (_x . _x))) . iterate go
    -- , sSolve = Just . fmap (fromJust . countFirstRepeat) . _
    -- , sSolve = Just . foldl1 lcm . fmap ((+) 1 .fromJust . countFirstRepeat) . (\xs -> V4 (map (view _1) xs) (map (view _2) xs) (map (view _3) xs) (map (view _4) xs))
    --          . map (fmap (view _x)) . iterate go
    , sSolve = Just . foldl1 lcm . fmap (fromJust . countFirstCycle) . (\xs -> SV.generate (\i -> map (`SV.index` i) xs))
             . map (fromJust . SV.fromList @12 . foldMap toList . fmap (view _x)) . iterate go
    -- , sSolve = countFirstRepeat
    -- -- foldl1 lcm . fmap ((+) 1 .fromJust . countFirstRepeat) . (\xs -> V4 (map (view _1) xs) (map (view _2) xs) (map (view _3) xs) (map (view _4) xs))
    --          . map (\ps -> V2 (S.fromList . toList $ (fmap (view _x) ps)) (S.fromList . toList $ fmap (view _y) ps)) . iterate go
    }
  where
    com ps = getSum . foldMap (Sum . view _x) $ ps
    -- com2 ps = getSum . foldMap (Sum . view _x) $ ps

    go ps = ps''
      where
        a   = getAccels ps
        ps' = (\(V2 x v) a -> V2 x (v + a)) <$> ps <*> a
        ps'' = fmap (\(V2 x v) -> V2 (x + v) v) ps'

-- recenter :: V4 Phase -> V4 Phase
-- recenter ps = sequenceA $ V2 (fmap (subtract minX) xs) (subtract minV <$> vs)
--   where
--     V2 xs vs = sequenceA ps
--     V2 minX _ = boundingBox xs
--     V2 minV _ = boundingBox vs
-- -- Returns @'V2' (V2 xMin yMin) (V2 xMax yMax)@.
-- -- boundingBox :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> V2 (g a)
-- -- boundingBox = (\(Ap mn, Ap mx) -> V2 (getMin <$> mn) (getMax <$> mx))
-- --             . foldMap1 (\p -> (Ap (Min <$> p), Ap (Max <$> p)))


countFirstCycle :: Eq a => [a] -> Maybe Int
countFirstCycle xs = firstJust go . drop 1 $ zip (inits xs) (tails xs)
  where
    go (as, bs) = length as <$ guard (and (zipWith (==) as bs))
    -- go (n, ys) = n <$ guard (take n xs == take n ys)

-- countFirstCycle xs = go 0 xs (drop 1 xs)
--   where
--     go 0 (x:xs) (y:ys)
--       | x == y = 


countFirstRepeat :: Ord a => [a] -> Maybe Int
countFirstRepeat = go S.empty 0
  where
    go seen n [] = Nothing
    go seen n (x:xs)
      | x `S.member` seen = Just n
      | otherwise         = go (x `S.insert` seen) (n + 1) xs
