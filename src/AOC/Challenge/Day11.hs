-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day11 (
    day11a
  , day11b
  ) where

import           AOC.Common                (Dir(..), dirPoint, Point, displayAsciiMap)
import           AOC.Common.Intcode        (Memory, parseMem, stepForeverAndDie, untilHalt)
import           AOC.Solver                ((:~>)(..))
import           Control.DeepSeq           (NFData)
import           Control.Monad             (forever)
import           Control.Monad.State       (MonadState, gets, execState, modify)
import           Data.Conduino             (Pipe, (.|), runPipe, awaitSurely, yield)
import           Data.Functor              ((<&>))
import           Data.Map                  (Map)
import           Data.Void                 (Void)
import           GHC.Generics              (Generic)
import qualified Data.Conduino.Combinators as C
import qualified Data.Map                  as M

data Hull = Hull { hDir :: Dir, hPos :: Point, hMap :: Map Point Color }
  deriving (Eq, Ord, Show, Generic)
instance NFData Hull

data Color = Black | White
  deriving (Eq, Ord, Enum, Show, Generic)
instance NFData Color

-- | Empty hull
emptyHull :: Hull
emptyHull = Hull North 0 M.empty

-- | Empty hull with a single colored item at the origin
singletonHull :: Color -> Hull
singletonHull c = Hull North 0 (M.singleton 0 c)

-- | The producer of signals.  Sends 0 or 1 by detecting color under
-- current position on 'Hull'.
sensor
    :: MonadState Hull m
    => Pipe () Int u m Void
sensor = C.repeatM . gets $ \(Hull _ p h) ->
    case M.lookup p h of
      Just White -> 1
      _          -> 0

-- | The consumer of signals.  Takes 0's and 1's to indicate color to paint
-- and direction to turn and step.
paint
    :: MonadState Hull m
    => Pipe Int Void Void m Void
paint = forever $ do
    color <- awaitSurely <&> \case
      0 -> Black
      1 -> White
      _ -> undefined
    turn  <- awaitSurely <&> \case
      0 -> West
      1 -> East
      _ -> undefined
    modify $ \(Hull d p h) -> Hull
        { hDir = d <> turn
        , hPos = p + dirPoint (d <> turn)
        , hMap = M.insert p color h
        }

-- | This is it
fullPipe
    :: MonadState Hull m
    => Memory
    -> Pipe () Void u m ()
fullPipe m = untilHalt $ sensor
                      .| stepForeverAndDie m
                      .| paint

day11a :: Memory :~> Int
day11a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> case execState (runPipe (fullPipe m)) emptyHull of
        Hull _ _ mp -> Just $ M.size mp
    }

day11b :: Memory :~> Map Point Color
day11b = MkSol
    { sParse = parseMem
    , sShow  = ("\n" <>)
             . unlines . reverse . lines
             . displayAsciiMap ' '
             . fmap (\case White -> '#'; Black -> ' ')
    , sSolve = \m -> case execState (runPipe (fullPipe m)) (singletonHull White) of
        Hull _ _ mp -> Just mp
    }

