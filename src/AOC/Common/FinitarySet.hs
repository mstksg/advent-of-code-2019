{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}

module AOC.Common.FinitarySet (
    FinitarySet(..)
  , empty, singleton, insert, delete, fromList, toList
  , intersection, union, difference, (\\)
  , isSubsetOf, isProperSubsetOf, disjoint
  , size, member, notMember, null
  , cartesianProduct, disjointUnion
  , foldr, foldr', foldl, foldl', map, foldMap, filter
  , alterF, generate, powerSet, mapMaybe
  , partition
  ) where

import           Control.DeepSeq                    (NFData)
import           Data.Bifunctor
import           Data.Bit
import           Data.Bits
import           Data.Finitary
import           Data.Finite
import           Data.Functor
import           GHC.Generics                       (Generic)
import           GHC.TypeNats
import           Prelude (Bool(..), Maybe(..), Either(..), Int, Monoid, Eq(..), Ord, Show, (&&), ($), (.), otherwise, Semigroup(..), not, fromIntegral, id)
import qualified Data.List                          as L
import qualified Data.Maybe                         as M
import qualified Data.Vector.Generic.Sized.Internal as VG
import qualified Data.Vector.Unboxed.Sized          as V
import qualified Prelude                            as P

newtype FinitarySet a = FinitarySet (V.Vector (Cardinality a) Bit)
  deriving (Show, Generic, Eq, Ord)

instance (Finitary a, KnownNat (2 ^ Cardinality a)) => Finitary (FinitarySet a)
instance NFData (FinitarySet a)

foldr :: Finitary a => (a -> b -> b) -> b -> FinitarySet a -> b
foldr f z (FinitarySet xs) =
    V.ifoldr (\i (Bit x) -> if x then f (fromFinite i) else id) z xs

foldr' :: Finitary a => (a -> b -> b) -> b -> FinitarySet a -> b
foldr' f z (FinitarySet xs) =
    V.ifoldr' (\i (Bit x) -> if x then f (fromFinite i) else id) z xs

foldl :: Finitary a => (b -> a -> b) -> b -> FinitarySet a -> b
foldl f z (FinitarySet xs) =
    V.ifoldl (\r i (Bit x) -> if x then f r (fromFinite i) else r) z xs

foldl' :: Finitary a => (b -> a -> b) -> b -> FinitarySet a -> b
foldl' f z (FinitarySet xs) =
    V.ifoldl' (\r i (Bit x) -> if x then f r (fromFinite i) else r) z xs

map :: (Finitary a, Finitary b) => (a -> b) -> FinitarySet a -> FinitarySet b
map f = fromList . fmap f . toList

foldMap :: (Finitary a, Monoid m) => (a -> m) -> FinitarySet a -> m
foldMap f = P.foldMap f . toList

toList :: Finitary a => FinitarySet a -> [a]
toList = foldr (:) []

empty :: KnownNat (Cardinality a) => FinitarySet a
empty = FinitarySet 0

-- could be made unsafe
singleton :: Finitary a => a -> FinitarySet a
singleton x = FinitarySet $ bit (fromIntegral (toFinite x))

fromList :: Finitary a => [a] -> FinitarySet a
fromList xs = FinitarySet $
    0 V.// fmap go xs
  where
    go x = (toFinite x, Bit True)

intersection :: FinitarySet a -> FinitarySet a -> FinitarySet a
intersection (FinitarySet (VG.Vector xs)) (FinitarySet (VG.Vector ys)) = FinitarySet (VG.Vector (xs .&. ys))

union :: FinitarySet a -> FinitarySet a -> FinitarySet a
union (FinitarySet (VG.Vector xs)) (FinitarySet (VG.Vector ys)) = FinitarySet (VG.Vector (xs .|. ys))

insert :: Finitary a => a -> FinitarySet a -> FinitarySet a
insert x (FinitarySet xs) = FinitarySet $ xs V.// [(toFinite x, Bit True)]

delete :: Finitary a => a -> FinitarySet a -> FinitarySet a
delete x (FinitarySet xs) = FinitarySet $ xs V.// [(toFinite x, Bit False)]

member :: Finitary a => a -> FinitarySet a -> Bool
member x (FinitarySet xs) = unBit $ xs `V.index` toFinite x

notMember :: Finitary a => a -> FinitarySet a -> Bool
notMember x = not . member x

null :: FinitarySet a -> Bool
null (FinitarySet (VG.Vector xs)) = popCount xs == 0

size :: FinitarySet a -> Int
size (FinitarySet (VG.Vector xs)) = popCount xs

isSubsetOf :: FinitarySet a -> FinitarySet a -> Bool
isSubsetOf (FinitarySet (VG.Vector xs)) (FinitarySet (VG.Vector ys)) = (xs .&. ys) == xs

isProperSubsetOf :: FinitarySet a -> FinitarySet a -> Bool
isProperSubsetOf (FinitarySet (VG.Vector xs)) (FinitarySet (VG.Vector ys)) =
      xs /= ys
   && (xs .&. ys) == xs

disjoint :: FinitarySet a -> FinitarySet a -> Bool
disjoint xs ys = null (xs `intersection` ys)

difference :: FinitarySet a -> FinitarySet a -> FinitarySet a
difference (FinitarySet (VG.Vector xs)) (FinitarySet (VG.Vector ys)) =
      FinitarySet (VG.Vector (xs .&. complement ys))

(\\) :: FinitarySet a -> FinitarySet a -> FinitarySet a
(\\) = difference

cartesianProduct
    :: (KnownNat (Cardinality a), KnownNat (Cardinality a * Cardinality b))
    => FinitarySet a
    -> FinitarySet b
    -> FinitarySet (a, b)
cartesianProduct (FinitarySet xs) (FinitarySet ys) = FinitarySet $ V.generate $ \i ->
    let (j, k) = separateProduct i
    in  (xs `V.index` j) .&. (ys `V.index` k)

disjointUnion :: FinitarySet a -> FinitarySet b -> FinitarySet (Either a b)
disjointUnion (FinitarySet (VG.Vector xs)) (FinitarySet (VG.Vector ys)) =
    FinitarySet (VG.Vector (xs <> ys))

partition
    :: Finitary a
    => (a -> Bool)
    -> FinitarySet a
    -> (FinitarySet a, FinitarySet a)
partition f = bimap fromList fromList . L.partition f . toList

mapMaybe
    :: (Finitary a, Finitary b)
    => (a -> Maybe b)
    -> FinitarySet a
    -> FinitarySet b
mapMaybe f = fromList . M.mapMaybe f . toList

powerSet
    :: (Finitary a, KnownNat (2 ^ Cardinality a))
    => FinitarySet a
    -> FinitarySet (FinitarySet a)
powerSet = fromList . fmap fromList . L.subsequences . toList

alterF
    :: (Finitary a, Functor f)
    => (Bool -> f Bool)
    -> a
    -> FinitarySet a
    -> f (FinitarySet a)
alterF f x xs
    | x `member` xs = f True <&> \case
        False -> x `delete` xs
        True  -> xs
    | otherwise     = f False <&> \case
        False -> xs
        True  -> x `insert` xs

generate :: Finitary a => (a -> Bool) -> FinitarySet a
generate f = FinitarySet $ V.generate (Bit . f . fromFinite)

filter
    :: Finitary a
    => (a -> Bool)
    -> FinitarySet a
    -> FinitarySet a
filter f (FinitarySet xs) = FinitarySet $ V.imap go xs
  where
    go i (Bit x) = Bit $ x && f (fromFinite i)


