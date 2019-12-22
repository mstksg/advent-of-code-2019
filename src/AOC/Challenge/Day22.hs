{-# LANGUAGE TypeApplications         #-}
{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day22
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 22.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day22 where
-- module AOC.Challenge.Day22 (
--     day22a
--   , day22b
--   ) where

import           AOC.Prelude
import           Data.Proxy
import           Data.Vector   (Vector)
import           GHC.TypeNats
import           Linear.Matrix
import           Linear.Vector
import qualified Data.Map      as M
import qualified Data.Set      as S
import qualified Data.Vector   as V

data Shuff = SCut Int
           | SIncr Int
           | SReverse
  deriving Show

parseLine :: String -> Shuff
parseLine xs = case words xs of
    "cut":n:_ -> SCut (read n)
    "deal":"into":_ -> SReverse
    "deal":"with":_:n:_ -> SIncr (read n)

type Mat = Map Int Int

shuffMat :: Num a => Int -> Shuff -> Mat
shuffMat n = \case
    SReverse -> M.fromList
      [ (i, n - i - 1)
      | i <- [0..n-1]
      ]
    SCut c -> M.fromList
      [ (i, (i + c) `mod` n)
      | i <- [0..n-1]
      ]
    SIncr c -> M.fromList
      [ ((i * c) `mod` n, i)
      | i <- [0..n-1]
      ]

compMat :: Mat -> Mat -> Mat
compMat m1 m2 = (m1 M.!) <$> m2

applyMat :: Mat -> Vector a -> Vector a
applyMat m xs = V.generate (V.length xs) $ \i -> xs V.! (m M.! i)

doShuff :: Num a => Vector a -> Shuff -> Vector a
doShuff xs shuff = V.generate n $ \i -> xs V.! (m M.! i)
  where
    n = V.length xs
    m = shuffMat n shuff

shuffOne :: Int -> Int -> Shuff -> Int
shuffOne n i = \case
    -- SReverse -> n - i - 1
    SReverse -> (-i - 1) `mod` n
    -- SReverse -> (-1-i)  `mod` n
    SCut c   -> (i - c) `mod` n
    SIncr c  -> (i * c) `mod` n

data ShuffPoly n = S { spA :: !(Finite n)
                     , spB :: !(Finite n)
                     }
  deriving (Eq, Ord, Show)

instance KnownNat n => Semigroup (ShuffPoly n) where
    S a2 b2 <> S a1 b1 = S (a2 * a1) (a2 * b1 + b2)
instance KnownNat n => Monoid (ShuffPoly n) where
    mempty = S 1 0

shuffPoly :: KnownNat n => Shuff -> ShuffPoly n
shuffPoly = \case
    SReverse -> S maxBound maxBound
    SCut c   -> S        1  (m (-c))
    SIncr c  -> S    (m c)        0
  where
    m = modulo . fromIntegral

applyPoly :: KnownNat n => ShuffPoly n -> Finite n -> Finite n
applyPoly (S a b) x = a * x + b

day22a :: _ :~> _
day22a = MkSol
    { sParse = Just . map parseLine . lines
    , sShow  = show
    , sSolve = \shuffs -> case someNatVal (dyno_ "n" 10007) of
                SomeNat (p@Proxy :: Proxy n) -> Just $
                  let bigShuff :: ShuffPoly n
                      bigShuff = mconcat $ reverse (shuffPoly <$> shuffs)
                  in  fromIntegral $ applyPoly bigShuff (dyno_ "i" 2019)
    }

type Part2 = 119315717514047
     -- of *`119315717514047` space cards*.

shuffPolyReverse :: KnownNat n => Shuff -> ShuffPoly n
shuffPolyReverse = \case
    SReverse -> S maxBound maxBound
    SCut c   -> S        1    (m c)
    SIncr c  -> S    (m (-c))     0
  where
    m = modulo . fromIntegral

invertPoly :: forall n. KnownNat n => ShuffPoly n -> ShuffPoly n
invertPoly (S a b) = S a' b'
  where
    a' = a ^ (maxBound @(Finite n) - 1)
    b' = negate (a' * b)


day22b :: _ :~> _
day22b = MkSol
    { sParse = Just . map parseLine . lines
    , sShow  = show
    , sSolve = \shuffs -> Just $
        let bigShuff :: ShuffPoly Part2
            bigShuff = mconcat $ reverse $ shuffPoly <$> shuffs
            repeated = stimes numReps bigShuff
        in  fromIntegral $ applyPoly (invertPoly repeated) 2020
    }
  where
    n       = 119315717514047
    numReps = 101741582076661
    -- , sSolve = \shuffs -> Just $
    --     let n :: Int
    --         n       = 119315717514047
    --         numReps = 101741582076661
    --         resFunc :: Int -> Int
    --         resFunc x = foldl' (shuffOne n) x shuffs
    --         bigShuff :: ShuffPoly
    --         bigShuff = foldr1 (compShuff n) . reverse $ shuffPoly <$> shuffs

    --         -- foldr1 (.) (flip (shuffOne n) <$> shuffs)
    --         res = resFunc 2020
    --         -- res = foldl' (shuffOne n) 2020 (concat . replicate 101741582076661 $ shuffs)
-- -- the deck *`101741582076661` times in a row*.
    --     in  firstRepeated . take 1000000 $ iterate resFunc 2020
    --     -- in  n - numReps
        -- in  firstRepeated . take 1000000 . (zip`ap`tail) $ iterate resFunc 0


            -- -- n = 1000000000
            -- xs :: Map Int Int
            -- xs = M.fromList . zip [0..] $ [ 0 .. n - 1 ]
            -- doMany :: Mat Int
            -- doMany = foldl1 (!*!) (shuffMat n <$> shuffs)
            -- res = foldl' (doShuff n) xs shuffs
        -- in  res M.! 2020
