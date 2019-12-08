{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Prelude

day08a :: String :~> Int
day08a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = fmap answer
             . minimumByMay (comparing (countTrue (== '0')))
             . chunksOf (dyno_ "w" 25 * dyno_ "h" 6)
    }
  where
    answer x  = countTrue (== '1') x * countTrue (== '2') x

day08b :: [String] :~> String
day08b = MkSol
    { sParse = Just . chunksOf 150
    , sShow  = unlines . chunksOf 25 . map (\case '0' -> ' '; _ -> '#')
    , sSolve = traverse (listToMaybe . dropWhile (== '2')) . transpose
    }
