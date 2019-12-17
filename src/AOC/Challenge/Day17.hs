{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day17 (
    day17a
  , day17b
  ) where

import           AOC.Common.Conduino
import           AOC.Common.Intcode
import           AOC.Prelude
import           Data.Conduino
import qualified Data.Map            as M
import qualified Data.Sequence       as Seq
import qualified Data.Set            as S

day17a :: _ :~> _
day17a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> case feedPipe [] (stepForever @IErr m) of
                       Left _ -> Nothing
                       Right (map chr -> os, _) -> Just $
                         let mp = parseAsciiMap charTile os
                             neighbs = M.keysSet $ M.filterWithKey go mp
                             go p Scaff = allScaff mp p
                             go _ _     = False
                         in  sum $ S.map (\(V2 x y) -> x * y) neighbs
    }

data Tile = Scaff | Robot Dir
  deriving (Show, Eq, Ord, Generic)

allScaff mp p = all (\q -> M.lookup q mp == Just Scaff) (cardinalNeighbs p)

-- cardinalNeighbs :: Point -> [Point]
-- cardinalNeighbs p = (p +) <$> [ V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0 ]

instance NFData Tile

charTile = \case
  '#' -> Just Scaff
  '^' -> Just $ Robot North
  '>' -> Just $ Robot East
  'v' -> Just $ Robot South
  '<' -> Just $ Robot West
  _ -> Nothing

tileChar = \case
  Scaff -> '#'
  Robot North -> '^'
  Robot East  -> '>'
  Robot South  -> 'v'
  Robot West  -> '<'


day17b :: _ :~> _
day17b = MkSol
    { sParse = parseMem
    , sShow  = show
    -- , sShow  = displayAsciiMap ' '
    -- , sShow  = id
    , sSolve = \m ->
        let m' = m & mRegLens 0 .~ 2
            inp = unlines
                ["A,B,A,C,B,C,B,C,A,C"
                ,"R,12,L,10,R,12"
                ,"L,8,R,10,R,6"
                ,"R,12,L,10,R,10,L,8"
                ,"n"
                ]
        in  case feedPipe (map ord inp) (stepForever @IErr m') of
              Right (o, _)   -> lastMay o
              _              -> Nothing

-- mRegLens :: Natural -> Lens' Memory Int
-- mRegLens i = #mRegs . at i . non 0

              -- neighbs = M.keysSet $ M.filterWithKey go mp
              -- go p Scaff = allScaff mp p
              -- go _ _     = False
          -- in  sum $ S.map (\(V2 x y) -> x * y) neighbs
          -- let mp = parseAsciiMap charTile os
          -- in  mp
              -- neighbs = M.keysSet $ M.filterWithKey go mp
              -- go p Scaff = allScaff mp p
              -- go _ _     = False
          -- in  sum $ S.map (\(V2 x y) -> x * y) neighbs
    }

getNodes scaff p = case ns of
    [x,y] -> guard (x /= (y <> South)) $> nss
    _     -> Just nss
  where
    ns = catMaybes $ zip [North,East,South,West] (cardinalNeighbs p) <&> \(d,q) -> 
        d <$ guard (q `S.member` scaff)
    nss = S.fromList ns
    -- n@[x] -> Just (S.fromList n)
    -- n@[x,y] -> guard (not $ any (== EQ) (liftA2 compare x y)) $> S.fromList [x,y]
    -- n@[_,_,_] -> Just (S.fromList n)
    -- n@[_,_,_,_] -> Just (S.fromList n)
    -- _   -> error "what"
