{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day15 (
    day15a
  , day15b
  ) where

import           AOC.Common            (Point, Dir(..), floodFillCount, dirPoint)
import           AOC.Common.Conduino   (feedPipe)
import           AOC.Common.Intcode    (Memory, VMErr, parseMem, stepForever, untilHalt)
import           AOC.Common.Search     (aStar)
import           AOC.Solver            ((:~>)(..))
import           AOC.Util              (maybeAlt)
import           Control.Applicative   (empty)
import           Control.DeepSeq       (NFData)
import           Data.Conduino         (Pipe)
import           Data.Functor.Identity (Identity(..))
import           Data.Semigroup        (Arg(..))
import           Data.Set              (Set)
import           Data.Void             (Void)
import           GHC.Generics          (Generic)
import           Safe                  (lastMay)
import qualified Data.Map              as M
import qualified Data.Set              as S

data Tile = Floor | Oxygen
  deriving (Eq, Ord, Show, Generic)
instance NFData Tile

data Spot = S
    { sCoord :: !Point
    , sTile  :: !Tile
    }
  deriving (Eq, Ord, Show, Generic)
instance NFData Spot

type Bot = Int -> Pipe Int Int Void Identity ()

-- | We use 'Arg' becase we only compare on the 'Spot', not the 'Bot'
type AState = Arg Spot Bot

-- | Since heuristic is 0 and weights are 1, this is basically just a BFS.
-- But I'm using 'aStar' because I don't want to implement it from scratch
-- heh
findOxygen :: Memory -> Maybe (Int, [AState])
findOxygen mem = aStar
    (const 0)
    (M.fromSet (const 1) . stepAround)
    (Arg (S 0 Floor) initBot)
    (\(Arg (S _ t) _) -> t == Oxygen)
  where
    initBot :: Bot
    initBot = c
      where
        (_, Left c) = runIdentity $ feedPipe [] (untilHalt (stepForever @VMErr mem))

stepAround :: AState -> Set AState
stepAround (Arg S{..} bot) = S.fromList $ do
    dir            <- [ North .. ]
    let p = sCoord + dirPoint dir
    (outs, Left c) <- pure . runIdentity $ feedPipe [] (bot (dNum dir))
    lastOut        <- maybeAlt $ lastMay outs
    case lastOut of
      1 -> pure $ Arg (S p Floor ) c
      2 -> pure $ Arg (S p Oxygen) c
      _ -> empty

dNum :: Dir -> Int
dNum = \case
  North -> 1
  East  -> 4
  South -> 2
  West  -> 3

day15a :: Memory :~> Int
day15a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = fmap fst . findOxygen
    }

day15b :: Memory :~> Int
day15b = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> do
        a0 <- lastMay . snd =<< findOxygen m
        Just . fst $ floodFillCount stepAround (S.singleton a0)
    }
