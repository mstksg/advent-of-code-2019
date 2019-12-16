-- |
-- Module      : AOC.Prelude
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Custom Prelude while developing challenges.  Ideally, once challenges
-- are completed, an import to this module would be replaced with explicit
-- ones for future readers.
--


module AOC.Prelude (
    module P
  ) where

import           AOC.Common                as P
import           AOC.Common.Search         as P
import           AOC.Solver                as P
import           AOC.Util                  as P
import           Control.Applicative       as P
import           Control.DeepSeq           as P
import           Control.Lens              as P hiding (uncons, Empty)
import           Control.Monad             as P
import           Control.Monad.Except      as P
import           Control.Monad.State       as P
import           Data.Bifunctor            as P
import           Data.Char                 as P
import           Data.Coerce               as P
import           Data.Containers.ListUtils as P
import           Data.Containers.NonEmpty  as P (withNonEmpty, nonEmpty)
import           Data.Either               as P
import           Data.Finite               as P (Finite, packFinite, getFinite, modulo, finites)
import           Data.Foldable             as P
import           Data.Function             as P
import           Data.Functor              as P
import           Data.IntMap               as P (IntMap)
import           Data.IntMap.NonEmpty      as P (NEIntMap)
import           Data.IntSet               as P (IntSet)
import           Data.IntSet.NonEmpty      as P (NEIntSet)
import           Data.Kind                 as P
import           Data.List                 as P
import           Data.List.NonEmpty        as P (NonEmpty(..))
import           Data.List.Split           as P
import           Data.Map                  as P (Map)
import           Data.Map.NonEmpty         as P (NEMap)
import           Data.Maybe                as P
import           Data.Ord                  as P
import           Data.Profunctor           as P (Profunctor(..))
import           Data.Semigroup            as P
import           Data.Sequence             as P (Seq(..))
import           Data.Sequence.NonEmpty    as P (NESeq(..))
import           Data.Set                  as P (Set)
import           Data.Set.NonEmpty         as P (NESet)
import           Data.Text                 as P (Text)
import           Data.Text.Encoding        as P (encodeUtf8, decodeUtf8)
import           Data.Time                 as P hiding (Day)
import           Data.Traversable          as P
import           Data.Tuple                as P
import           Data.Void                 as P
import           Debug.Trace               as P
import           GHC.Generics              as P (Generic)
import           Linear                    as P (V1(..), V2(..), V3(..), V4(..))
import           Numeric.Natural           as P
import           Safe                      as P hiding (at, maximumDef, maximumNote, maximumMay, maximumByDef, maximumByNote, maximumByMay, minimumDef, minimumNote, minimumMay, minimumByDef, minimumByNote, minimumByMay, foldr1Def, foldr1May, foldr1Note, foldl1Def, foldl1May, foldl1Note, findJust, findJustDef, findJustNote)
import           Safe.Foldable             as P
import           Text.Printf               as P
import           Text.Read                 as P (readMaybe)
