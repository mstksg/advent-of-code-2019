-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import           AOC.Common.Intcode        (Memory(..), IErr(..), parseMem, stepForever)
import           AOC.Common.Search         (binaryMinSearch)
import           AOC.Solver                ((:~>)(..), dyno_)
import           AOC.Util                  (eitherToMaybe)
import           Control.Monad.Except      (throwError)
import           Data.Conduino             (runPipe, (.|), (|.))
import qualified Data.Conduino.Combinators as C
import qualified Data.Map                  as M

setMem :: Maybe Int -> Maybe Int -> Memory -> Memory
setMem noun verb m = m { mRegs = maybe id (M.insert 2) verb
                               . maybe id (M.insert 1) noun
                               $ mRegs m
                       }


runProg :: Memory -> Maybe Int
runProg m = eitherToMaybe . runPipe $
        throwError IENoInput
     .| ((M.! 0) . mRegs <$> stepForever m)
     |. C.sinkNull

day02a :: Memory :~> Int
day02a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = runProg
             . setMem (Just (dyno_ "noun" 12)) (Just (dyno_ "verb" 2))
    }

day02b :: Memory :~> (Int, Int)
day02b = MkSol
    { sParse = parseMem
    , sShow  = \(noun,verb) -> show $ noun * 100 + verb
    , sSolve = \m -> do
        -- for my code, noun makes big jumps and verb makes small ones
        -- search for noun first
        noun <- binaryMinSearch (\i ->
            (> Just moon) . runProg . setMem (Just (i + 1)) Nothing $ m
          ) 0 99
        let m' = setMem (Just noun) Nothing m
        -- search for verb next
        verb <- binaryMinSearch (\j ->
            (> Just moon) . runProg . setMem Nothing (Just (j + 1)) $ m'
          ) 0 99
        pure (noun, verb)
    }
  where
    moon = 19690720

