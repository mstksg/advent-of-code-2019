{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : AOC.Challenge.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day23 (
    day23a
  , day23b
  ) where

import           AOC.Common          (Point)
import           AOC.Common.Conduino (feedPipe)
import           AOC.Common.Intcode  (Memory, parseMem, VM, stepForever, VMErr)
import           AOC.Solver          ((:~>)(..))
import           AOC.Util            (firstJust)
import           Control.Lens        (view, (%%~), at)
import           Control.Monad       (guard, ap)
import           Data.Function       ((&))
import           Data.List.Split     (chunksOf)
import           Data.Map            (Map)
import           Data.Maybe          (mapMaybe, catMaybes)
import           Data.Sequence       (Seq(..))
import           Data.Traversable    (for)
import           Linear.V2           (V2(..), _y)
import qualified Data.Map            as M
import qualified Data.Sequence       as Seq

data MultiMem = MM
    { mmPipes :: !(Map Int (Int -> VM (Either VMErr) Memory))
    , mmQueue :: !(Seq (Int, Point))       -- ^ use one big global queue
    , mmNAT   :: !(Maybe Point)
    }

initMultiMem :: Memory -> MultiMem
initMultiMem m = MM
    { mmPipes = M.fromList (catMaybes pipes')
    , mmQueue = parseOuts outList
    , mmNAT   = Nothing
    }
  where
    (outList, pipes') = for [0..49] $ \i ->
      case feedPipe [i] (stepForever @VMErr m) of
        Left _ -> ([], Nothing)
        Right (os, r) -> case r of
          Left  n -> (os, Just (i, n))
          Right _ -> (os, Nothing    )


stepMultiMem :: MultiMem -> MultiMem
stepMultiMem mm@MM{..} = case mmQueue of
    Empty -> case mmNAT of
      Just a  -> mm { mmQueue = Seq.singleton (0, a) }
      Nothing ->
        let (outList, pipes') = fmap (M.mapMaybe id) . for mmPipes $ \n ->
              case feedPipe [] (n (-1)) of
                Left  e -> error $ show e
                Right (os, r) -> case r of
                  Left n' -> (os, Just n')
                  Right _ -> (os, Nothing)
        in  mm { mmPipes = pipes', mmQueue = parseOuts outList }
    (i, p@(V2 x y)) :<| ps
      | i == 255  -> mm { mmNAT = Just p, mmQueue = ps }
      | otherwise ->
          let (outList, pipes') = mmPipes & at i %%~ \case
                Nothing -> ([], Nothing)
                Just n  -> case feedPipe [y] (n x) of
                  Left  _ -> ([], Nothing)
                  Right (os, r) -> case r of
                    Left n' -> (os, Just n')
                    Right _ -> (os, Nothing)
              queue' = ps <> parseOuts outList
          in  MM pipes' queue' mmNAT

parseOuts :: [a] -> Seq (a, V2 a)
parseOuts = Seq.fromList . mapMaybe splitOut . chunksOf 3
  where
    splitOut [i,x,y] = Just (i, V2 x y)
    splitOut _       = Nothing

day23a :: Memory :~> Int
day23a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = firstJust (firstJust find255 . mmQueue)
             . iterate stepMultiMem
             . initMultiMem
    }
  where
    find255 (255, V2 _ y) = Just y
    find255 _             = Nothing

day23b :: Memory :~> Int
day23b = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = firstJust (\(x,y) -> x <$ guard (x == y))
             . (zip`ap`tail)
             . mapMaybe natted
             . iterate stepMultiMem
             . initMultiMem
    }
  where
    natted MM{..} = do
      guard $ Seq.null mmQueue
      view _y <$> mmNAT

