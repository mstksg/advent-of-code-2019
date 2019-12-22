{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : AOC.Challenge.Day22
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 22.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day22 (
    day22a
  , day22b
  ) where

import           AOC.Solver      ((:~>)(..))
import           Control.DeepSeq (NFData)
import           Data.Finite     (Finite, modulo)
import           Data.Group      (Group(..))
import           GHC.Generics    (Generic)
import           GHC.TypeNats    (KnownNat)
import           Text.Read       (readMaybe)

data Affine n = Aff { affA :: !(Finite n)
                    , affB :: !(Finite n)
                    }
  deriving (Eq, Ord, Show, Generic, NFData)

instance KnownNat n => Semigroup (Affine n) where
    Aff a2 b2 <> Aff a1 b1 = Aff (a2 * a1) (a2 * b1 + b2)
instance KnownNat n => Monoid    (Affine n) where
    mempty = Aff 1 0
instance KnownNat n => Group (Affine n) where
    invert (Aff a b) = Aff a' b'
      where
        a' = recipFin a
        b' = negate (a' * b)



(@$) :: KnownNat n => Affine n -> Finite n -> Finite n
Aff a b @$ x = a * x + b

data Shuff n = SCut  (Finite n)
             | SIncr (Finite n)
             | SReverse
  deriving (Eq, Ord, Show, Generic, NFData)

shuffAff :: KnownNat n => Shuff n -> Affine n
shuffAff = \case
    SReverse -> Aff (negate 1) (negate 1)
    SCut c   -> Aff         1  (negate c)
    SIncr c  -> Aff         c          0


day22a :: [Shuff 10007] :~> Int
day22a = MkSol
    { sParse = fmap reverse . traverse parseLine . lines
    , sShow  = show
    , sSolve = \shuffs -> fmap fromIntegral . Just $
            foldMap shuffAff shuffs @$ 2019
    }

day22b :: [Shuff 119315717514047] :~> Int
day22b = MkSol
    { sParse = fmap reverse . traverse parseLine . lines
    , sShow  = show
    , sSolve = \shuffs -> fmap fromIntegral . Just $
        let bigShuff    = foldMap shuffAff shuffs
        in  (bigShuff `pow` (-numReps)) @$ 2020
    }
  where
    numReps :: Int
    numReps = 101741582076661

parseLine :: KnownNat n => String -> Maybe (Shuff n)
parseLine xs = case words xs of
    "cut":n:_           -> SCut  . modulo <$> readMaybe n
    "deal":"into":_     -> Just SReverse
    "deal":"with":_:n:_ -> SIncr . modulo <$> readMaybe n
    _                   -> Nothing

recipFin :: forall n. KnownNat n => Finite n -> Finite n
recipFin x = x ^ (maxBound @(Finite n) - 1)
