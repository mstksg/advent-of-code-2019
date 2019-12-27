-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day14 (
    day14a
  , day14b
  ) where

import           AOC.Common         (clearOut, loopEither)
import           AOC.Common.Search  (exponentialMinSearch)
import           AOC.Solver         ((:~>)(..))
import           Control.DeepSeq    (NFData)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.List.Split    (chunksOf)
import           Data.Map           (Map)
import           Data.Map.NonEmpty  (NEMap)
import           GHC.Generics       (Generic)
import           Text.Read          (readMaybe)
import qualified Data.Map           as M
import qualified Data.Map.NonEmpty  as NEM

type Recipes = Map String (Int, NEMap String Int)

parseRecipeLine :: String -> Maybe Recipes
parseRecipeLine = (agg . reverse =<<)
                . traverse parseChunk
                . chunksOf 2
                . words
                . clearOut (`elem` (",=>" :: String))
  where
    parseChunk [readMaybe->Just x, y] = Just (y, x)
    parseChunk _                      = Nothing
    agg ((x,c):k:ks) = Just $ M.singleton x (c, NEM.fromList (k :| ks))
    agg _            = Nothing

data Basket = B { bOre    :: Int
                , bNeed   :: NEMap String Int
                , bExcess :: Map String Int
                }
  deriving (Show, Eq, Ord, Generic)
instance NFData Basket

splitCosts :: NEMap String Int -> (Int, Map String Int)
splitCosts = NEM.alterF (\x -> (sum x,Nothing)) "ORE"

splitBasket :: Recipes -> Basket -> Either (Int, Map String Int) Basket
splitBasket rs B{..} = case need' of
    NEM.IsEmpty           -> Left (bOre + newOre, newExc)
    NEM.IsNonEmpty needNE -> Right $ B
      { bOre    = bOre + newOre
      , bNeed   = needNE
      , bExcess = newExc
      }
  where
    ((ingr, amt), rest) = NEM.deleteFindMin bNeed
    (amt', excess') = case M.lookup ingr bExcess of
      Nothing -> (Just amt, bExcess)
      Just exc -> case compare amt exc of
        GT -> (Just $ amt - exc, M.delete ingr bExcess)
        EQ -> (Nothing         , M.delete ingr bExcess)
        LT -> (Nothing         , M.insert ingr (exc - amt) bExcess)
    (newOre, need', newExc) = case amt' of
      Nothing -> (0, rest, excess')
      Just a  ->
        let (quant, costs) = rs M.! ingr
            buyAmt   = (a + quant - 1) `div` quant
            leftover = (buyAmt * quant) - a
            (o, c')
                | buyAmt   == 0 = (0, M.empty)
                | otherwise     = splitCosts $ fmap (* buyAmt) costs
            exc | leftover == 0 = excess'
                | otherwise     = M.insertWith (+) ingr leftover excess'
        in  (o, M.unionWith (+) c' rest, exc)

oreForFuel :: Recipes -> Int -> Int
oreForFuel rs i = fst . loopEither (splitBasket rs) $ B
    { bOre    = 0
    , bNeed   = NEM.singleton "FUEL" i
    , bExcess = M.empty
    }

day14a :: Recipes :~> Int
day14a = MkSol
    { sParse = foldMap parseRecipeLine . lines
    , sShow  = show
    , sSolve = Just . (`oreForFuel` 1)
    }

day14b :: Recipes :~> Int
day14b = MkSol
    { sParse = foldMap parseRecipeLine . lines
    , sShow  = show
    , sSolve  = \rs -> subtract 1 <$>
        exponentialMinSearch (\fuel -> oreForFuel rs fuel > 1e12) 1
    }

