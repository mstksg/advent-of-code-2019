-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day07 (
    day07a
  , day07b
  ) where

import           AOC.Common.Conduino       (evalStateP, feedbackP, runExceptP)
import           AOC.Common.Intcode        (Memory, VM, untilHalt, stepForeverAndDie, parseMem)
import           AOC.Solver                ((:~>)(..))
import           Control.Monad.Except      (MonadError, throwError)
import           Data.Conduino             (Pipe, (.|), yield, runPipePure, runPipe, awaitSurely)
import           Data.List                 (permutations)
import           Data.Void                 (Void)
import           Safe                      (lastMay, maximumMay)
import qualified Data.Conduino.Combinators as C

yieldAndDie :: MonadError String m => o -> Pipe i o u m a
yieldAndDie i = yield i *> throwError "that's all you get"

yieldAndPass :: o -> Pipe o o u m u
yieldAndPass i = yield i *> C.map id

setupChain :: MonadError String m => Memory -> [Int] -> VM m Void
setupChain m = foldr ((.|) . prime) (C.map id)
  where
    prime i = yieldAndPass i
           .| evalStateP m stepForeverAndDie

day07a :: _ :~> _
day07a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> maximumMay
            [ r
            | xs <- permutations [0..4]
            , Right r <- (:[]) $
                runPipe $ yieldAndDie 0
                       .| setupChain m xs
                       .| awaitSurely
            ]
    }

day07b :: _ :~> _
day07b = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> maximumMay
            [ r
            | xs <- permutations [5..9]
            , Just r <- (:[]) $
                runPipePure $ untilHalt ( yieldAndDie 0
                                       .| feedbackP (setupChain m xs)
                                        )
                           .| C.last
            ]
    }

