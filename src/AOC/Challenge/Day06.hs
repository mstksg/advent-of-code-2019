{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import           AOC.Prelude
import qualified Data.Tree as Tr
import qualified Data.Map as M

-- type T = Tree

parset :: String -> Map String String
parset = M.fromList . map (\(splitOn")"->[x,y])->(y, x)) . lines

day06a :: Map String String :~> Int
day06a = MkSol
    { sParse = Just . parset
    , sShow  = show
    , sSolve = \t -> let sumMap = flip M.mapWithKey t $ \k v ->
                            case M.lookup v sumMap of
                              Nothing -> 1
                              Just s  -> s + 1
                     in  Just $ sum sumMap
    }

-- go t x = sumOf

day06b :: _ :~> _
day06b = MkSol
    { sParse = Just . parset
    , sShow  = show
    , sSolve = \t -> let treeUp s = case M.lookup s t of
                                      Nothing -> []
                                      Just k  -> k : treeUp k
                         youTree = reverse $ treeUp "YOU"
                         sanTree = reverse $ treeUp "SAN"
                         (pY, pS) = dropPre youTree sanTree
                     in  Just $ length pY + length pS
    }

dropPre [] xs = ([], xs)
dropPre  xs [] = (xs, [])
dropPre (x:xs) (y:ys)
    | x == y    = dropPre xs ys
    | otherwise = (x:xs, y:ys)






