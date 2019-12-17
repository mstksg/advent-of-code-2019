{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day17 (
    day17a
  , day17b
  ) where

import           AOC.Common.Conduino
import           AOC.Common.Intcode
import           AOC.Prelude
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map            as M
import qualified Data.Set            as S

data Tile = Scaff | Robot Dir
  deriving (Show, Eq, Ord, Generic)
instance NFData Tile

data AState = AS { asPos :: Point
                 , asDir :: Dir
                 }
  deriving (Show, Eq, Ord, Generic)
instance NFData AState

parseMap :: Memory -> Maybe (Set Point, Maybe AState)
parseMap m = do
    (os, _) <- eitherToMaybe $ feedPipe [] (stepForever @VMErr m)
    let mp    = parseAsciiMap parseTile (map chr os)
        scaff = M.keysSet mp
        sOut  = do
          (p, d) <- listToMaybe . M.toList . M.mapMaybe id $ mp
          pure (AS p d)
    pure (scaff, sOut)
  where
    parseTile = \case
      '#' -> Just Nothing
      '^' -> Just (Just North)
      '>' -> Just (Just East)
      'v' -> Just (Just South)
      '<' -> Just (Just West)
      _   -> Nothing


day17a :: Memory :~> Int
day17a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = fmap (sum . S.map product . findNeighbs . fst) . parseMap
    }
  where
    findNeighbs scaff = S.filter allScaff scaff
      where
        allScaff = all (`S.member` scaff) . cardinalNeighbs

day17b :: Memory :~> _
day17b = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \(set (mRegLens 0) 2 -> m) -> do
        (scaff, as0) <- sequenceA =<< parseMap m
        let path  = findPath scaff as0
        (a,b,c) <- findProgs path
        let mainProg = chomp [(a,"A"),(b,"B"),(c,"C")] path
            inp      = map ord . unlines . map (intercalate ",") $
              [ mainProg
              , showPC <$> a
              , showPC <$> b
              , showPC <$> c
              , ["n"]
              ]

        output <- fst <$> eitherToMaybe (feedPipe inp (stepForever @IErr m))
        lastMay output
    }


findProgs :: Eq a => [a] -> Maybe ([a], [a], [a])
findProgs p0 = listToMaybe $ do
    a <- validPrefix p0

    let withoutA = neSplitOn a p0
    b <- case withoutA of
        []        -> empty
        bs : _    -> validPrefix bs

    let withoutB = neSplitOn b =<< withoutA
    c <- case withoutB of
        []        -> empty
        c  : rest -> c <$ guard (all (== c) rest)

    pure (a, b, c)
  where
    neSplitOn x = filter (not . null) . splitOn x
    validPrefix = take 4 . filter (not . null) . inits

chomp :: Eq a => [([a], b)] -> [a] -> [b]
chomp progs = unfoldr go
  where
    go xs = asum
      [ (r,) <$> stripPrefix prog xs
      | (prog, r) <- progs
      ]



data PathComp = PC { pcTurn :: Bool, pcStep :: Int }
  deriving (Show, Eq, Ord, Generic)
instance NFData PathComp

showPC :: PathComp -> String
showPC PC{..} = (if pcTurn then "R" else "L") ++ "," ++ show pcStep

findPath :: Set Point -> AState -> [PathComp]
findPath scaff = mapMaybe process . chunksOf 2 . NE.group . unfoldr go
  where
    process [Just b:|_, steps] = Just $ PC b (length steps)
    process _                  = Nothing
    go AS{..}
        | forward   `S.member` scaff = Just (Nothing   , AS forward asDir          )
        | turnLeft  `S.member` scaff = Just (Just False, AS asPos   (asDir <> West))
        | turnRight `S.member` scaff = Just (Just True , AS asPos   (asDir <> East))
        | otherwise                  = Nothing
      where
        forward   = asPos + dirPoint' asDir
        turnLeft  = asPos + dirPoint' (asDir <> West)
        turnRight = asPos + dirPoint' (asDir <> East)
