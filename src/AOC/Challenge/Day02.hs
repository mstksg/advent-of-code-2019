{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

import           AOC.Common          (loopMaybe)
import           AOC.Common.Search   (exponentialMinSearch)
import           AOC.Solver          ((:~>)(..), dyno_)
import           Control.Applicative (empty)
import           Data.List.Split     (splitOn)
import           Data.Sequence       (Seq)
import           Linear              (V2(..), V3(..))
import           Text.Read           (readMaybe)
import qualified Data.Map            as M
import qualified Data.Sequence       as Seq

data Memory = Mem
    { mPos  :: Int
    , mRegs :: Seq Int
    }
  deriving Show

step :: Memory -> Maybe Memory
step (Mem p r) = do
    x  <- Seq.lookup p r
    o <- case x of
      1 -> pure (+)
      2 -> pure (*)
      _ -> empty
    V3 a b c  <- traverse (`Seq.lookup` r) (V3 p p p + V3 1 2 3)
    V2 y z    <- traverse (`Seq.lookup` r) (V2 a b)
    pure $ Mem (p + 4) (Seq.update c (o y z) r)

setMem :: Maybe Int -> Maybe Int -> Memory -> Memory
setMem noun verb m = m { mRegs = maybe id (Seq.update 2) verb
                               . maybe id (Seq.update 1) noun
                               $ mRegs m
                       }

runProg :: Memory -> Maybe Int
runProg = Seq.lookup 0 . mRegs . loopMaybe step

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
        noun <- flip exponentialMinSearch 1 $ \i ->
            (> Just moon) . runProg . setMem (Just (i + 1)) Nothing $ m
        let m' = setMem (Just noun) Nothing m
        -- search for verb next
        verb <- flip exponentialMinSearch 1 $ \j ->
            (> Just moon) . runProg . setMem Nothing (Just (j + 1)) $ m'
        pure (noun, verb)
    }
  where
    moon = 19690720

parseMem :: String -> Maybe Memory
parseMem = fmap (Mem 0 . Seq.fromList) . traverse readMaybe . splitOn ","

