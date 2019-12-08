{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Common.Intcode        (Memory, parseMem, yieldAndDie, stepForever)
import           AOC.Solver                ((:~>)(..))
import           Data.Conduino             ((.|), runPipe)
import           Data.Either               (fromRight)
import qualified Data.Conduino.Combinators as C

runProg :: Int -> Memory -> Either String (Maybe Int)
runProg i m = runPipe $ yieldAndDie i
                     .| stepForever m
                     .| C.last

day05a :: Memory :~> Int
day05a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = fromRight Nothing . runProg 1
    }

day05b :: Memory :~> Int
day05b = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = fromRight Nothing . runProg 5
    }
