{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}


module AOC.Common.Subset (
    findSubset
  , testFinder
  ) where

import           AOC.Util
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.Foldable
import           Data.List.NonEmpty        (NonEmpty(..))
import           Data.Map                  (Map)
import           Data.Ord
import           Data.Set                  (Set)
import qualified Data.List.NonEmpty        as NE
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Set.NonEmpty         as NES

-- | Get the number of guesses needed for each possible subset, for
-- n items.
testFinder :: Int -> Map (Set Int) Int
testFinder n = M.fromSet (\x -> getSum . execWriter $ findSubset (go x) xs) $ S.powerSet xs
  where
    xs  = S.fromList [0 .. n - 1]
    go goal x = compare (sumSet x) goalAmt <$ tell (Sum 1)
      where
        goalAmt = sumSet goal
    sumSet :: Set Int -> Int
    sumSet = getSum . foldMap (Sum . (2 ^))

attrEntropy :: Ord a => Set (Set a) -> Set a -> Double
attrEntropy xs x = pLT * hLT + pGT * hGT
  where
    ltWeight = 2 * lt + unknown
    gtWeight = 2 * gt + unknown
    pLT      = ltWeight / (fromIntegral (S.size xs) * 2)
    pGT      = gtWeight / (fromIntegral (S.size xs) * 2)
    hLT      = lt * entroRecip (ltWeight/2) + unknown * entroRecip ltWeight
    hGT      = gt * entroRecip (gtWeight/2) + unknown * entroRecip gtWeight
    (Sum lt, Sum gt, Sum _, Sum unknown) = flip foldMap xs $ \y ->
      if | y == x                   -> (mempty, mempty, Sum (1 :: Int), mempty)
         | x `S.isProperSubsetOf` y -> (Sum 1 , mempty, mempty, mempty)
         | y `S.isProperSubsetOf` x -> (mempty, Sum 1 , mempty, mempty)
         | otherwise                -> (mempty, mempty, mempty, Sum 1 )

filterTest :: Ord a => Set (Set a) -> Set a -> Ordering -> Set (Set a)
filterTest xs x = \case
    LT -> flip S.filter xs $ \y -> not $ y `S.isSubsetOf` x
    EQ -> S.filter (== x) xs
    GT -> flip S.filter xs $ \y -> not $ x `S.isSubsetOf` y

findSubset
    :: (Monad m, Ord a)
    => (Set a -> m Ordering)        -- ^ tester
    -> Set a                        -- ^ set of items
    -> m (Maybe (Set a))            -- ^ subset that matches tester
findSubset tester = runMaybeT . go . S.powerSet
  where
    go xs = do
        (subset, _) <- maybeAlt $
            minimumBy (comparing snd) <$> NE.nonEmpty entropies
        NES.IsNonEmpty rest <- filterTest xs subset <$> lift (tester subset)
        let res :| others = NES.toList rest
        if null others
          then pure res
          else go (NES.toSet rest)
      where
        entropies = M.toList $ M.fromSet (attrEntropy xs) xs

entroRecip :: Double -> Double
entroRecip 0 = 0
entroRecip p = -(1/p) * log (1/p)
