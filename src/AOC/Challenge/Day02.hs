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

import           AOC.Common               (loopMaybe)
import           AOC.Common.Search        (binaryMinSearch)
import           AOC.Solver               ((:~>)(..), dyno_)
import           Control.Applicative      (empty)
import           Data.Containers.NonEmpty (onNonEmpty)
import           Data.List.Split          (splitOn)
import           Data.Sequence.NonEmpty   (NESeq)
import           Linear                   (V2(..), V3(..))
import           Text.Read                (readMaybe)
import qualified Data.Sequence.NonEmpty   as NESeq

-- TODO: we need to implement fuseBoth for conduino if we want to refactor

data Memory = Mem
    { mPos  :: Int
    , mRegs :: NESeq Int
    }
  deriving Show

step :: Memory -> Maybe Memory
step (Mem p r) = do
    o <- NESeq.lookup p r >>= \case
      1 -> pure (+)
      2 -> pure (*)
      _ -> empty
    V3 a b c  <- traverse (`NESeq.lookup` r) (V3 p p p + V3 1 2 3)
    V2 y z    <- traverse (`NESeq.lookup` r) (V2 a b)
    pure $ Mem (p + 4) (NESeq.update c (o y z) r)

setMem :: Maybe Int -> Maybe Int -> Memory -> Memory
setMem noun verb m = m { mRegs = maybe id (NESeq.update 2) verb
                               . maybe id (NESeq.update 1) noun
                               $ mRegs m
                       }

runProg :: Memory -> Int
runProg = NESeq.head . mRegs . loopMaybe step

day02a :: Memory :~> Int
day02a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = Just
             . runProg
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
            (> moon) . runProg . setMem (Just (i + 1)) Nothing $ m
          ) 0 99
        let m' = setMem (Just noun) Nothing m
        -- search for verb next
        verb <- binaryMinSearch (\j ->
            (> moon) . runProg . setMem Nothing (Just (j + 1)) $ m'
          ) 0 99
        pure (noun, verb)
    }
  where
    moon = 19690720

parseMem :: String -> Maybe Memory
parseMem = (onNonEmpty (Mem 0 . NESeq.fromList) =<<)
         . traverse readMaybe
         . splitOn ","

