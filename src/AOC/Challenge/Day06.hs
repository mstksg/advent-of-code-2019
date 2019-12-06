-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import           AOC.Solver      ((:~>)(..))
import           Data.Functor    ((<&>))
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as M
import qualified Data.Set        as S

parseParents :: String -> Map String String
parseParents str = M.fromList [
      (y, x)
    | ln    <- lines str
    , [x,y] <- [splitOn ")" ln]
    ]

day06a :: Map String String :~> Int
day06a = MkSol
    { sParse = Just . parseParents
    , sShow  = show
    , sSolve = \parents -> Just $
        let orbits :: Map String Int
            orbits = parents <&> \v ->
               case M.lookup v orbits of
                 Nothing -> 1
                 Just s  -> s + 1
        in  sum orbits
    }

day06b :: Map String String :~> Int
day06b = MkSol
    { sParse = Just . parseParents
    , sShow  = show
    , sSolve = \parents -> do
        let orbits :: Map String [String]
            orbits = parents <&> \v ->
               case M.lookup v orbits of
                 Nothing -> []
                 Just ss -> v:ss
        you <- S.fromList <$> M.lookup "YOU" orbits
        san <- S.fromList <$> M.lookup "SAN" orbits
        let onlyYou = you S.\\ san
            onlySan = san S.\\ you
        pure $ S.size onlyYou + S.size onlySan
    }

