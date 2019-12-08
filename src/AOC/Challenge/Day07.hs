-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day07 (
    day07a
  , day07b
  ) where

import           AOC.Common.Conduino       (feedbackP)
import           AOC.Common.Intcode        (Memory, VM, untilHalt, stepForeverAndDie, parseMem, yieldAndDie, yieldAndPass)
import           AOC.Solver                ((:~>)(..))
import           AOC.Util                  (eitherToMaybe)
import           Control.Monad.Except      (MonadError)
import           Data.Conduino             ((.|), runPipePure, runPipe, awaitSurely)
import           Data.List                 (permutations)
import           Data.Semigroup            (Max(..))
import           Data.Void                 (Void)
import qualified Data.Conduino.Combinators as C

setupChain :: MonadError String m => Memory -> [Int] -> VM m Void
setupChain m = foldr ((.|) . prime) (C.map id)
  where
    prime i = yieldAndPass i
           .| stepForeverAndDie m

day07a :: Memory :~> Int
day07a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> fmap getMax . flip foldMap (permutations [0..4]) $ \xs ->
        let res = runPipe $ yieldAndDie 0
                         .| setupChain m xs
                         .| awaitSurely
        in  Max <$> eitherToMaybe res
    }

day07b :: Memory :~> Int
day07b = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> fmap getMax . flip foldMap (permutations [5..9]) $ \xs ->
        let res = runPipePure $ untilHalt ( yieldAndDie 0
                                         .| feedbackP (setupChain m xs)
                                          )
                            .| C.last
        in  Max <$> res
    }

