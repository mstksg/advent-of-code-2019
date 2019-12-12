-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day12 (
    day12a
  , day12b
  ) where

import           AOC.Common           ((!!!), clearOut)
import           AOC.Solver           ((:~>)(..), dyno_)
import           AOC.Util             (firstJust)
import           Control.Lens         (view)
import           Control.Monad        (guard)
import           Data.Char            (isDigit)
import           Data.List            (inits, tails)
import           Data.Semigroup       (Sum(..))
import           Linear hiding        (transpose)
import           Text.Read            (readMaybe)

type Phase = V2
type Point = V3 Int

parsePos :: String -> Maybe (Phase Point)
parsePos str = do
    [x,y,z] <- traverse readMaybe . words . clearOut p $ str
    pure $ V2 (V3 x y z) 0
  where
    p '-' = False
    p c   = not (isDigit c)

getAccels
    :: Num a
    => V4 (Phase a)
    -> V4 a
getAccels xs = fmap acc xs
  where
    acc (V2 x _) = getSum
                 . foldMap (Sum . signum . subtract x . view _x)
                 $ xs

step
    :: Num a
    => V4 (Phase a)
    -> V4 (Phase a)
step ps = update <$> ps <*> getAccels ps
  where
    update (V2 x v) a = V2 (x + v') v'
      where
        v' = v + a
      
day12a :: V4 (Phase Point) :~> Int
day12a = MkSol
    { sParse = \str -> do
        [a,b,c,d] <- traverse parsePos . lines $ str
        pure $ V4 a b c d
    , sShow  = show
    , sSolve = Just . getSum
             . foldMap (Sum . energy)
             . (!!! dyno_ "steps" 1000)
             . iterate step
    }
  where
    energy (V2 x v) = sum (abs x) * sum (abs v)
        

-- here we run three independent simulations of 4 one-dimensional planets
day12b :: V3 (V4 (Phase Int)) :~> Int
day12b = MkSol
    { sParse = \str -> do
        [a,b,c,d] <- traverse parsePos . lines $ str
        pure . traverse sequenceA $ V4 a b c d
    , sShow  = show
    , sSolve = fmap (foldl1 lcm)
             . traverse (countFirstCycle . iterate step)
    }

countFirstCycle :: Eq a => [a] -> Maybe Int
countFirstCycle xs = firstJust go . drop 1 $ zip (inits xs) (tails xs)
  where
    go (as, bs) = length as <$ guard (and (zipWith (==) as bs))
