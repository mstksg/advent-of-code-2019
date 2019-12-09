{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day09 (
    day09a
  , day09b
  ) where

import           AOC.Common.Intcode (Memory, yieldAndDie, stepForever, parseMem)
import           AOC.Solver         ((:~>)(..))
import           AOC.Util           (eitherToMaybe)
import           Control.Monad      (join)
import           Data.Conduino      (runPipe, (.|), await)

runProg :: Int -> Memory -> Either String (Maybe Int)
runProg i m = runPipe $ yieldAndDie i
                     .| stepForever m
                     .| await

day09a :: _ :~> _
day09a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = join . eitherToMaybe . runProg 1
    }

day09b :: _ :~> _
day09b = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = join . eitherToMaybe . runProg 2
    }
