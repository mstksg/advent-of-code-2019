{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day20 (
    day20a
  , day20b
  ) where

import           AOC.Prelude
import           Data.Generics.Labels ()
import           Linear.Metric
import           GHC.Natural
import           Linear.Vector
import qualified Data.Map             as M
import qualified Data.Sequence        as Seq
import qualified Data.Set             as S
import qualified Data.Set.NonEmpty    as NES

data Edge = Inner | Outer
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)
instance NFData Edge

otherEdge :: Edge -> Edge
otherEdge = \case
    Inner -> Outer
    Outer -> Inner

data Edges a = Edges { eInner :: a, eOuter :: a }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
instance NFData a => NFData (Edges a)

data Portal = P { pLabel :: String, pEdge :: Edge }
  deriving (Eq, Ord, Generic)
instance NFData Portal

instance Show Portal where
    showsPrec _ P{..} = showString pLabel
                      . showString (case pEdge of Inner -> "i"; Outer -> "o")

data Maze = Maze
    { mWalls   :: Set Point
    , mPortals :: Map Point Portal
    , mLinks   :: Map String (Edges Point)
    , mAA      :: Point
    , mZZ      :: Point
    }
  deriving (Show, Eq, Ord, Generic)
instance NFData Maze

type PortalMap = Map Portal Int

portalsFrom :: Maze -> Point -> PortalMap
portalsFrom Maze{..} = go 1 mWalls
  where
    go !dist seen p = addPortal
                    . M.unionsWith min
                    $ map (go (dist + 1) seen') neighbs
      where
        neighbs    = S.toList $ cardinalNeighbsSet p `S.difference` seen
        seen'      = S.insert p seen
        portalHere = M.lookup p mPortals
        addPortal  = case portalHere of
                       Nothing -> id
                       Just c  -> M.insertWith min c dist

data PortalToPortal = PTP
    { ptpPortals :: Map String (Edges PortalMap)
    , ptpAA      :: PortalMap
    }
  deriving (Eq, Ord, Generic)
instance NFData PortalToPortal

showPortalMap :: PortalMap -> String
showPortalMap = intercalate ", " . map go . M.toList
  where
    go (p, i) = printf "[%2d]%s" i (show p)

instance Show PortalToPortal where
    show PTP{..} = unlines $
          ("AAo: " ++ showPortalMap ptpAA)
        : concatMap go (M.toList ptpPortals)
      where
        go (p, Edges i o) =
            [ p ++ "i: " ++ showPortalMap i
            , p ++ "o: " ++ showPortalMap o
            ]




portalToPortal :: Maze -> PortalToPortal
portalToPortal mz@Maze{..} = PTP{..}
  where
    ptpPortals = M.mapWithKey deleteSelf $ fmap (portalsFrom mz) <$> mLinks
    ptpAA      = M.delete (P "AA" Outer) $ portalsFrom mz mAA
    deleteSelf lab = fmap (`M.withoutKeys` S.fromList [P lab Inner, P lab Outer])

pNeighbs1 :: PortalToPortal -> String -> Map String Int
pNeighbs1 PTP{..} p = M.mapKeysMonotonic pLabel portalMap
  where
    portalMap = case p of
      "AA" -> ptpAA
      pp   -> M.unionsWith min . foldMap (:[]) $ ptpPortals M.! pp

day20a :: _ :~> _
day20a = MkSol
    { sParse = parseMaze
    , sShow  = show
    , sSolve = \mz -> subtract 1 . fst <$> aStar
                        (const 0)
                        (pNeighbs1 (portalToPortal mz))
                        "AA"
                        (== "ZZ")
    }


data AState = AS
    { asPortal :: !String
    , asEdge   :: !Edge
    , asLevel  :: !(Maybe Natural)
    }
  deriving (Eq, Ord, Generic)
instance NFData AState

instance Show AState where
    showsPrec _ AS{..} = showString "<"
                       . showString asPortal
                       . showString (case asEdge of Inner -> "i"; Outer -> "o")
                       . showString ","
                       . showString (maybe "x" show asLevel)
                       . showString ">"


pNeighbs2 :: PortalToPortal -> AState -> Map AState Int
pNeighbs2 PTP{..} AS{..} = M.fromList
    [ (AS lab (otherEdge e) l', c)
    | (P lab e, c) <- M.toList portalMap
    , lab /= "ZZ" || asLevel == Just 0
    , lab /= "AA"
    , l' <- case e of
        Inner -> pure $ Just . maybe 0 (+1) $ asLevel
        Outer
          | lab == "ZZ" -> pure Nothing
          | otherwise   -> fmap Just . maybeToList $ asLevel >>= (`minusNaturalMaybe` 1)
    ]
  where
    portalMap = case asPortal of
      "AA" -> ptpAA
      pp   -> case asEdge of
        Inner -> eInner $ ptpPortals M.! pp
        Outer -> eOuter $ ptpPortals M.! pp

day20b :: _ :~> _
day20b = MkSol
    { sParse = parseMaze
    , sShow  = show
    , sSolve = \mz -> subtract 1 . fst <$> aStar
                        (maybe 0 ((+1) . fromIntegral) . asLevel)
                        (pNeighbs2 (portalToPortal mz))
                        (AS "AA" Outer (Just 0))
                        (AS "ZZ" Inner Nothing ==)
    }

data Tile = TFloor
          | TWall
          | TPortal Char
  deriving (Show, Eq, Ord, Generic)

parseMaze :: String -> Maybe Maze
parseMaze str = do
    V2 mins maxs <- boundingBox' mFloors
    let fromCenter = distance (fmap fromIntegral (mins + maxs) ^/ (2 :: Double)) . fmap fromIntegral
        mLinks = flip M.mapMaybe mLinks_ $ \ps -> case NES.toList ps of
          p1 :| [p2]
            | fromCenter p1 < fromCenter p2 -> Just $ Edges p1 p2
            | otherwise                     -> Just $ Edges p2 p1
          _          -> Nothing


    mAA <- NES.findMin <$> M.lookup "AA" mLinks_
    mZZ <- NES.findMin <$> M.lookup "ZZ" mLinks_

    let endPortals = M.fromList [(mAA, P "AA" Outer), (mZZ, P "ZZ" Outer)]
        mPortals   = M.union endPortals . M.fromList . concatMap splitPortal . M.toList $ mLinks

    pure $ Maze{..}
  where
    mp = flip parseAsciiMap str $ \case
            '.' -> Just TFloor
            '#' -> Just TWall
            c   -> TPortal c <$ guard (isAlpha c)
    mWalls    = M.keysSet . M.filter (/= TFloor) $ mp
    mFloors   = M.keysSet . M.filter (== TFloor) $ mp
    mPSpecs   = M.mapMaybe (preview #_TPortal) mp
    mPortals_ = M.mapMaybe id . M.fromSet findPortal $ mFloors
    mLinks_   = M.fromListWith (<>) . map (second NES.singleton . swap) . M.toList $ mPortals_
    findPortal p = asum $ map findDir [North ..]
      where
        findDir d = ((== 2) . length) `mfilter` Just (toList letters)
          where
            dp      = dirPoint d
            letters = mPSpecs `M.restrictKeys` S.fromList [p+dp, p+2*dp]
    splitPortal (p, e) = [(eInner e, P p Inner), (eOuter e, P p Outer)]


testPoints :: Set AState
testPoints = S.fromList
    [ AS "AA" Outer (Just 0)
    , AS "XF" Outer (Just 1)
    , AS "CK" Outer (Just 2)
    , AS "ZH" Outer (Just 3)
    , AS "WB" Outer (Just 4)
    , AS "IC" Outer (Just 5)
    , AS "RF" Outer (Just 6)
    , AS "NM" Outer (Just 7)
    , AS "LP" Outer (Just 8)
    , AS "FD" Outer (Just 9)
    , AS "XQ" Outer (Just 10)
    , AS "WB" Inner (Just 9)
    , AS "ZH" Inner (Just 8)
    , AS "CK" Inner (Just 7)
    , AS "XF" Inner (Just 6)
    , AS "OA" Inner (Just 5)
    , AS "CJ" Inner (Just 4)
    , AS "RE" Inner (Just 3)
    , AS "IC" Outer (Just 4)
    , AS "RF" Outer (Just 5)
    , AS "NM" Outer (Just 6)
    , AS "LP" Outer (Just 7)
    , AS "FD" Outer (Just 8)
    , AS "XQ" Outer (Just 9)
    , AS "WB" Inner (Just 8)
    , AS "ZH" Inner (Just 7)
    , AS "CK" Inner (Just 6)
    , AS "XF" Inner (Just 5)
    , AS "OA" Inner (Just 4)
    , AS "CJ" Inner (Just 3)
    , AS "RE" Inner (Just 2)
    , AS "XQ" Inner (Just 1)        -- this is it: it should go to fd inner, not inner
    , AS "FD" Inner (Just 0)
    ]



