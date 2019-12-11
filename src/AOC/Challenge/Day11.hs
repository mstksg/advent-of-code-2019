{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day11 (
    day11a
  , day11b
  ) where

import           AOC.Common.Conduino
import           AOC.Common.Intcode
import           AOC.Prelude
import           Control.Monad.State
import           Data.Conduino
import           Data.Functor
import qualified Data.Conduino.Combinators as C
import qualified Data.Map                  as M

data Hull = Hull { hDir :: Dir, hPos :: Point, hMap :: Map Point Bool }

emptyHull = Hull North 0 M.empty
emptyHull2 = Hull North 0 (M.singleton 0 True)

sensor :: MonadState Hull m => Pipe () Int u m Void
sensor = C.repeatM $ do
    gets $ \(Hull _ p h) -> case M.lookup p h of
                    Nothing -> 0
                    Just False -> 0
                    Just True   -> 1


paint :: MonadState Hull m => Pipe Int Void Void m ()
paint = do
    color <- (== 1) <$> awaitSurely
    turn  <- awaitSurely <&> \case
      0 -> West
      1 -> East
      _ -> undefined
    modify $ \(Hull d p h) -> Hull (d <> turn) (p + dirPoint (d <> turn)) (M.insert p color h)
    paint

fullPipe :: (MonadState Hull m, MonadError IErr m) => Memory -> Pipe () Void u m ()
fullPipe m = sensor
          .| stepForeverAndDie m
          .| paint

day11a :: _ :~> _
day11a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> case runState (runExceptT (runPipe (fullPipe m))) emptyHull of
                        (_, Hull _ _ m) -> Just $ M.size m
    }

day11b :: _ :~> _
day11b = MkSol
    { sParse = parseMem
    , sShow  = ("\n"<>) . unlines . reverse . lines . displayAsciiMap ' ' . fmap (\case True -> '#'; False -> ' ')
-- displayAsciiMap
--     :: Char             -- ^ default tile
--     -> Map Point Char   -- ^ tile map
--     -> String
    , sSolve = \m -> case runState (runExceptT (runPipe (fullPipe m))) emptyHull2 of
                        (_, Hull _ _ m) -> Just m
    }

