{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -w                    #-}

-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day18 (
    day18a
  , day18b
  ) where

import           AOC.Prelude
import           Data.Functor.Rep     as R
import           Data.Generics.Labels
import           Linear.V4
import           Linear.Vector
import qualified Data.Map             as M
import qualified Data.Set             as S

data Maze f = Maze
    { mWalls  :: Set Point
    , mKeys   :: Map Point F26
    , mDoors  :: Map Point F26
    , mKeyLoc :: Map F26  Point
    , mStart  :: f Point
    }
  deriving (Generic)
deriving instance Eq (f Point) => Eq (Maze f)
deriving instance Ord (f Point) => Ord (Maze f)
deriving instance Show (f Point) => Show (Maze f)
instance NFData (f Point) => NFData (Maze f)

-- | From a given point, a map to every visible key, with the distance and
-- the set of keys and doors in the way.
type KeyMap = Map F26 (Int, Set F26)

-- | Do a DFS to build the key map
keysFrom :: Maze f -> Point -> KeyMap
keysFrom Maze{..} = go 0 mWalls S.empty
  where
    go !dist seen doors p = addKey
                          . M.unionsWith better
                          $ map (go (dist + 1) seen' doors') neighbs
      where
        neighbs  = S.toList $ cardinalNeighbsSet p `S.difference` seen
        seen'    = S.insert p seen
        doors'   = addDoor $ case M.lookup p mDoors of
                     Nothing -> doors
                     Just d  -> S.insert d doors
        keyHere  = M.lookup p mKeys
        addDoor  = case keyHere of
                     Nothing -> id
                     Just c  -> S.insert c
        addKey   = case keyHere of
                     Nothing -> id
                     Just c  -> M.insertWith better c (dist, doors)
    better (a,x) (b,y)
      | b < a     = (b, y)
      | otherwise = (a, x)


data KeyToKey f = K
    { kStart :: f KeyMap
    , kKeys  :: Map F26 KeyMap
    }
  deriving (Generic)
deriving instance Show (f KeyMap) => Show (KeyToKey f)
instance NFData (f KeyMap) => NFData (KeyToKey f)

keyToKey :: Functor f => Maze f -> KeyToKey f
keyToKey mz@Maze{..} = K
    { kStart = keysFrom mz <$> mStart
    , kKeys  = M.mapWithKey (\c -> M.delete c . keysFrom mz) mKeyLoc
    }

data AState f = AS
    { aKeys :: !(Set F26)
    , aPos  :: !(f (Maybe F26))
    }
  deriving (Generic)
deriving instance Eq (f (Maybe F26)) => Eq (AState f)
deriving instance Ord (f (Maybe F26)) => Ord (AState f)
instance NFData (f (Maybe F26)) => NFData (AState f)

aHeuristic :: Maze f -> AState f -> Int
aHeuristic Maze{..} AS{..} = M.size mKeyLoc - S.size aKeys

aStep
    :: forall f. (Foldable f, Representable f, Rep f ~ E f, Ord (AState f))
    => KeyToKey f
    -> AState f
    -> Map (AState f) Int
aStep K{..} AS{..} = M.fromList
    [ (AS aKeys' aPos', cost)
    | e <- toList $ tabulate @f id
    , let p = aPos ^. el e
    , (goal, (cost, doors)) <- M.toList $ case p of
        Nothing -> kStart ^. el e
        Just c  -> kKeys M.! c
    , goal `S.notMember` aKeys
    , S.null $ doors `S.difference` aKeys
    , let aKeys' = S.insert goal aKeys
          aPos'  = aPos & el e ?~ goal
    ]


day18a :: Maze V1 :~> Int
day18a = MkSol
    { sParse = parseMaze
    , sShow  = show
    , sSolve = \mz -> fst <$>
        aStar (aHeuristic mz)
              (aStep (keyToKey mz))
              (AS S.empty (pure Nothing))
              ((== 0) . aHeuristic mz)
    }

reMaze :: Maze V1 -> Maze V4
reMaze m@Maze{..} = m
    { mWalls = S.union (cardinalNeighbsSet p0) . S.insert p0 $ mWalls
    , mStart = (+p0) <$> V4 (V2 (-1) (-1))
                            (V2   1  (-1))
                            (V2 (-1)   1 )
                            (V2   1    1 )
    }
  where
    V1 p0  = mStart

day18b :: Maze V4 :~> Int
day18b = MkSol
    { sParse = fmap reMaze . parseMaze
    , sShow  = show
    , sSolve = \mz -> fst <$>
        aStar (aHeuristic mz)
              (aStep (keyToKey mz))
              (AS S.empty (pure Nothing))
              ((== 0) . aHeuristic mz)
    }


-- utilities

instance Foldable f => Show (AState f) where
    showsPrec _ AS{..} =
            showString "AS<"
          . showString (foldMap ((:[]) . dispKey) aKeys)
          . showString ","
          . showString (foldMap ((:[]) . maybe '@' dispKey) aPos)
          . showString ">"

dispKey :: F26 -> Char
dispKey  = review _CharFinite . (False,)
dispDoor :: F26 -> Char
dispDoor = review _CharFinite . (True,)


dispKeyMap :: KeyMap -> String
dispKeyMap = intercalate ", " . map go . M.toList
  where
    go (c, (d, xs)) = printf "%c:%d[%s]" (dispKey c) d (foldMap ((:[]).dispDoor) xs)

data Item = IKey    F26
          | IDoor   F26
          | IWall
  deriving (Eq, Ord, Show, Generic)
instance NFData Item

toMaze :: Map Point Item -> Point -> Maze V1
toMaze mp p = Maze{..}
  where
    mWalls  = M.keysSet . M.filter (== IWall) $ mp
    mKeys   = M.mapMaybe (preview #_IKey ) mp
    mDoors  = M.mapMaybe (preview #_IDoor) mp
    mKeyLoc = M.fromList . map swap . M.toList $ mKeys
    mStart  = V1 p

parseMap :: String -> (Map Point Item, Maybe Point)
parseMap str = second (fmap getFirst) . swap
              . flip M.traverseMaybeWithKey mp
              $ \p -> \case
                  Nothing -> (Just (First p), Nothing)
                  Just t  -> (mempty        , Just t )
  where
    mp = flip parseAsciiMap str $ \case
      '#' -> Just $ Just IWall
      '@' -> Just Nothing
      c   -> charFinite c <&> \(up, d) -> Just $
               if up then IDoor d
                     else IKey  d

parseMaze :: String -> Maybe (Maze V1)
parseMaze = fmap (uncurry toMaze) . sequenceA . parseMap
