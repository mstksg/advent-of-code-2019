{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- |
-- Module      : AOC.Challenge.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day21 (
    day21a
  , day21b
  ) where

import           AOC.Common                (_CharFinite)
import           AOC.Common.Intcode        (Memory, parseMem, untilHalt, stepForever, IErr, preAscii)
import           AOC.Solver                ((:~>)(..))
import           Control.Applicative       (empty)
import           Control.Lens              (review)
import           Control.Monad             ((<=<))
import           Data.Char                 (ord)
import           Data.Conduino             (Pipe, runPipe, (.|), yield)
import           Data.Finite               (Finite, weakenN)
import           Data.List                 (find)
import           Data.Text                 (Text)
import           Data.Void                 (Void)
import           Text.Printf               (printf)
import qualified Data.Conduino.Combinators as C
import qualified Data.Text                 as T


data Reg = RTemp | RJump | RInp (Finite 9)
  deriving (Eq, Ord, Show)

data Com = CAnd | COr | CNot
  deriving (Eq, Ord, Show)

data Instr = I Com Reg Reg
  deriving (Eq, Ord, Show)

type Program = [Instr]

runSpringbot
    :: Monad m
    => Pipe () Text u m Void
    -> Memory
    -> m [Int]
runSpringbot src m = runPipe $ src
                            .| preAscii
                            .| untilHalt (stepForever @IErr m)
                            .| C.sinkList

sourceWalk :: Program -> Pipe i Text u m ()
sourceWalk p = do
    C.sourceList (instrCode <$> p)
    yield "WALK"

theProg :: Program
theProg = [
      I COr (RInp 3) RJump      -- jump if target is stable
    , I CNot RTemp RTemp        -- RTemp == True
    , I CAnd (RInp 0) RTemp     -- RTemp &&= r0
    , I CAnd (RInp 1) RTemp     -- RTemp &&= r1
    , I CAnd (RInp 2) RTemp     -- RTemp &&= r2
    , I CNot RTemp RTemp        -- then don't jump
    , I CAnd RTemp RJump
    ]

-- the logic:
--
-- jump whenever it is safe to do so.  but don't jump frivolously (if there
-- is no hole in sight)
--
-- 0 ####   No
-- 1 ###.   No
-- 2 ##.#   Yes
-- 3 ##..   No
-- 4 #.##   Yes
-- 5 #.#.   No
-- 6 #..#   Yes
-- 7 #...   No
-- 8 .###   Yes
-- 9 .##.   Give Up
-- a .#.#   Yes
-- b .#..   Give Up
-- c ..##   Yes
-- d ..#.   Give Up
-- e ...#   Yes
-- f ....   Give Up
--

day21a :: Memory :~> Int
day21a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = isGood <=< runSpringbot (sourceWalk theProg *> empty)
    }

sourceRun :: Program -> Pipe i Text u m ()
sourceRun p = do
    C.sourceList (instrCode <$> p)
    yield "RUN"

-- the logic:
--
-- the same but also try to 'double jump' if you can.
theProg2 :: Program
theProg2 = [
      I CNot (RInp 0) RJump     -- jump if next spot is bad
    , I COr  (RInp 3) RTemp      -- jump if target is stable
    , I CAnd (RInp 7) RTemp      --   and double-target is stable
    , I COr  RTemp    RJump
    , I CNot (RInp 0) RTemp
    , I CNot RTemp    RTemp
    , I CAnd (RInp 1) RTemp     -- RTemp &&= r1
    , I CAnd (RInp 2) RTemp     -- RTemp &&= r2
    , I CNot RTemp RTemp        -- then don't jump
    , I CAnd RTemp RJump
    ]


day21b :: Memory :~> Int
day21b = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = isGood <=< runSpringbot (sourceRun theProg2 *> empty)
    }

isGood :: [Int] -> Maybe Int
isGood = find (> ord maxBound)

regCode :: Reg -> Char
regCode = \case
    RTemp -> 'T'
    RJump -> 'J'
    RInp c -> review _CharFinite (True, weakenN c)

comCode :: Com -> String
comCode = \case
    CAnd -> "AND"
    COr  -> "OR"
    CNot -> "NOT"

instrCode :: Instr -> Text
instrCode (I c x y) = T.pack $ printf "%s %c %c" (comCode c) (regCode x) (regCode y)

