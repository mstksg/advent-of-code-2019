{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

module AOC.Challenge.Day18 where

-- module AOC.Challenge.Day18 (
--     day18a
--   , day18b
--   ) where

import           AOC.Prelude
import           Data.Tree     (Tree(..), Forest)
import           Linear.V4
import           Linear.Vector
import qualified Data.Map      as M
import qualified Data.Sequence as Seq
import qualified Data.Set      as S
import qualified Data.Tree     as Tr

data Item = IKey (Finite 26)
          | IDoor (Finite 26)
          | IChar
  deriving (Eq, Ord, Show, Generic)

data AState = A { aKeys  :: Set (Finite 26)
                , aPos   :: Point
                }
  deriving (Eq, Ord, Show, Generic)

aHeuristic
    :: Set (Finite 26)
    -> AState
    -> Int
aHeuristic allKeys A{..} = S.size $ allKeys `S.difference` aKeys

type F26 = Finite 26

aNeighb
    :: Set Point    -- ^ walls
    -> Map Point (Finite 26)   -- ^ doors
    -> Map Point (Finite 26)  -- ^ keys
    -> AState
    -> Set AState
aNeighb walls doors keys A{..} = S.fromList
    [ A keys' p
    | p <- cardinalNeighbs aPos
    , p `S.notMember` illegal
    , let keys' = case M.lookup p keys of
                     Nothing -> aKeys
                     Just c  -> c `S.insert` aKeys
    ]
  where
    locked   = M.keysSet $ M.filter (`S.notMember` aKeys) doors
    illegal  = walls `S.union` locked
    
flipMap :: Ord a => Map k a -> Map a k
flipMap = M.fromList . map swap . M.toList

day18a :: _ :~> _
day18a = MkSol
    { sParse = Just . parseAsciiMap pr
    , sShow  = show . fst
    , sSolve = \mp ->
        let walls   = M.keysSet . M.filter isNothing $ mp
            keys    = M.mapMaybe (\case Just (IKey c) -> Just c; _ -> Nothing) mp
            allKeys = S.fromList . toList $ keys
            doors   = M.mapMaybe (\case Just (IDoor c) -> Just c; _ -> Nothing) mp
            (p0,_)  = M.findMin $ M.filter (== Just IChar) mp
        in  aStar (aHeuristic allKeys)
                  (M.fromSet (const 1) . aNeighb walls doors keys)
                  (A S.empty p0)
                  (\a -> aHeuristic allKeys a == 0)

    }
  where
    pr = \case
      '#' -> Just Nothing
      '@' -> Just $ Just IChar
      c -> charFinite c <&> \(up, d) ->
                if up then Just $ IDoor d
                      else Just $ IKey  d
             

-- charFinite :: Char -> Maybe (Bool, Finite 26)

data AState2 = A2 { a2Keys  :: !(Set (Finite 26))
                  , a2Doors :: !(Set Point)
                  , a2Pos   :: !(V4 Point)
                  }
  deriving (Eq, Ord, Show, Generic)

aHeuristic2
    :: Set (Finite 26)
    -> AState2
    -> Int
aHeuristic2 allKeys A2{..} = S.size $ allKeys `S.difference` a2Keys

aNeighb2
    :: Set Point    -- ^ walls
    -> Map (Finite 26) Point    -- ^ doors
    -> Map Point (Finite 26)  -- ^ keys
    -> AState2
    -> Set AState2
aNeighb2 walls doors keys A2{..} = S.fromList
    [ A2 keys' doors' (a2Pos & el e .~ p)
    -- A keys' p
    | e <- [ex, ey, ez, ew]
    , p <- cardinalNeighbs (a2Pos ^. el e)
    , p `S.notMember` illegal
    , let (keys',doors') =
            case M.lookup p keys of
              Nothing -> (a2Keys, a2Doors)
              Just c  -> (c `S.insert` a2Keys, case M.lookup c doors of Nothing -> a2Doors; Just d -> d `S.delete` a2Doors)
    ]
  where
    illegal  = walls `S.union` a2Doors

-- data MiniState = MS { msPos :: V2 Point }

aNeighb2'
    :: Set Point    -- ^ walls
    -> Map (Finite 26) Point    -- ^ doors
    -> Map (Finite 26) Point   -- ^ keys
    -> Map Point (Finite 26)  -- ^ keys
    -> AState2
    -> Map AState2 Int
aNeighb2' walls doors allKeys keys A2{..} = M.fromList
    [ (A2 keys' doors' (a2Pos & el e .~ rawPos), found)
    -- A2 keys' doors' (a2Pos & el e .~ p)
    -- A keys' p
    | e       <- [ex, ey, ez, ew]
    , (rawGoal, rawPos) <- M.toList (allKeys `M.withoutKeys` a2Keys)
    -- , rawGoal `S.notMember` a2Keys
    , let p0     = a2Pos ^. el e
          keys'  = rawGoal `S.insert` a2Keys
          doors' = case M.lookup rawGoal doors of
                     Nothing -> a2Doors
                     Just d  -> d `S.delete` a2Doors
    , found <- maybeToList . fmap fst $ aStar
                          (mannDist rawPos)
                          (M.fromSet (const 1) . (`S.difference` illegal) . S.fromList . cardinalNeighbs)
                          p0
                          (== rawPos)
    ]
  where
    illegal  = walls `S.union` a2Doors

enactPlan :: Set Point -> [AState2] -> Int
enactPlan walls = sum . map go . (zip`ap`tail)
  where
    go (a1, a2) = case liftA2 compare (a2Pos a1) (a2Pos a2) of
      V4 EQ EQ EQ EQ -> error "hey"
      V4 _  EQ EQ EQ -> fst . fromJust $ aStar (mannDist (a2Pos a2 ^. _x))
                              (M.fromSet (const 1) . (`S.difference` walls) . S.fromList . cardinalNeighbs)
                              (a2Pos a1 ^. _x)
                              (== (a2Pos a2 ^. _x))
      V4 EQ _  EQ EQ -> fst . fromJust $ aStar (mannDist (a2Pos a2 ^. _y))
                              (M.fromSet (const 1) . (`S.difference` walls) . S.fromList . cardinalNeighbs)
                              (a2Pos a1 ^. _y)
                              (== (a2Pos a2 ^. _y))
      V4 EQ EQ _  EQ -> fst . fromJust $ aStar (mannDist (a2Pos a2 ^. _z))
                              (M.fromSet (const 1) . (`S.difference` walls) . S.fromList . cardinalNeighbs)
                              (a2Pos a1 ^. _z)
                              (== (a2Pos a2 ^. _z))
      V4 EQ EQ EQ _  -> fst . fromJust $ aStar (mannDist (a2Pos a2 ^. _w))
                              (M.fromSet (const 1) . (`S.difference` walls) . S.fromList . cardinalNeighbs)
                              (a2Pos a1 ^. _w)
                              (== (a2Pos a2 ^. _w))

     

-- day18b :: _ :~> _
-- day18b = MkSol
--     { sParse = Just . parseAsciiMap pr
--     , sShow  = ("\n"++) . displayAsciiMap ' ' . fmap drawItem
--     -- , sShow  = show
--     -- , sShow  = show . second (map (`caeser` 'a') . foldMap S.toList . map (uncurry (flip S.difference)) . (zip `ap` tail) . map a2Keys)
--     , sSolve = \(pp->(mp, p0)) ->
--         let walls    = M.keysSet . M.filter isNothing $ mp
--             keys     = M.mapMaybe (\case Just (IKey c) -> Just c; _ -> Nothing) mp
--             allKeys  = S.fromList . toList $ keys
--             doors    = M.mapMaybe (\case Just (IDoor c) -> Just c; _ -> Nothing) mp
--             allDoors = M.keysSet doors
--             doorMap  = flipMap doors
--             keyMap   = flipMap keys
--             [pA, pB, pC, pD] = M.keys $ M.filter (== Just IChar) mp
--             -- quadrants = flip M.foldMapWithKey keys $ \k v ->
--             --     case liftA2 compare k of
--             --       V2 LT LT -> 
--         in  Just mp
--     -- -> V4 (Map (Finite 26) Point)   -- ^ keys
--         -- in  aStar (aHeuristic2 allKeys)
--         --           (aNeighb2' walls doorMap keyMap keys)
--         --           (A2 S.empty allDoors $ V4 pA pB pC pD)
--         --           (\a -> aHeuristic2 allKeys a == 0)
--         -- in  enactPlan walls . snd <$> aStar (aHeuristic2 allKeys)
--         --           (aNeighb2' walls doorMap keyMap keys)
--         --           (A2 S.empty allDoors $ V4 pA pB pC pD)
--         --           (\a -> aHeuristic2 allKeys a == 0)
--         -- in  aStar (aHeuristic2 allKeys)
--         --           (M.fromSet (const 1) . aNeighb2 walls doorMap keys)
--         --           (A2 S.empty allDoors $ V4 pA pB pC pD)
--         --           (\a -> aHeuristic2 allKeys a == 0)
--     }
--   where
--     pp mp = (M.unions [M.singleton p0 Nothing, newWalls, newPlayers, mp], p0)
--       where
--         (p0,_)  = M.findMin $ M.filter (== Just IChar) mp
--         newWalls = M.fromList (map (,Nothing) $ cardinalNeighbs p0)
--         newPlayers = M.fromList (map (,Just IChar) $ diagos)
--         diagos = map (+p0) [V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1)]
--     pr = \case
--       '#' -> Just Nothing
--       '@' -> Just $ Just IChar
--       c -> charFinite c <&> \(up, d) ->
--                 if up then Just $ IDoor d
--                       else Just $ IKey  d
--     drawItem = \case
--       Nothing -> '#'
--       Just IChar -> '@'
--       Just (IKey c) -> review _CharFinite (False, c)
--       Just (IDoor c) -> review _CharFinite (True, c)

flattenMaze :: Set Point -> Map Point a -> Point -> Forest (Int, Maybe a)
flattenMaze walls labels p00 = flattenForest . pruneForest isJust $ go (S.singleton p00) p00
  where
    go seen p0 =
      [ Node (M.lookup p labels) (go seen' p)
      | p <- S.toList . (`S.difference` seen) . (`S.difference` walls) . S.fromList . cardinalNeighbs $ p0
      , let seen' = S.insert p seen
      ]


flattenForest :: forall a. Forest (Maybe a) -> Forest (Int, Maybe a)
flattenForest = concatMap (go 1)
  where
    go i (Node Nothing [])  = []
    go i (Node Nothing [x]) = go (i + 1) x
    go i (Node Nothing [x,y])  = [Node (i, Nothing) (flattenForest [x,y])]
    go i (Node Nothing [x,y,z])  = [Node (i, Nothing) (flattenForest [x,y,z])]
    go i (Node (Just x) xs) = [Node (i, Just x) (flattenForest xs)]


type TreeIx = Seq Int

data AState3 = A3 { a3Keys  :: !(Set (Finite 26))
                  , a3Pos   :: !(V4 TreeIx)
                  }
  deriving (Eq, Ord, Generic)

instance Show AState3 where
    showsPrec _ A3{..} = showString "A3< "
                       . showString (foldMap ((:[]) . review _CharFinite . (False,)) a3Keys)
                       . showString "\t, "
                       . showsPrec 9 (toList <$> a3Pos)
                       . showString " >"

type ATree = Tree (Int, Maybe (Bool, Finite 26))

ixTree :: TreeIx -> Tree a -> Maybe a
ixTree Empty      (Node x _ ) = Just x
ixTree (i :<| is) (Node _ xs) = ixTree is =<< (xs !? i)

descendTree :: TreeIx -> Tree a -> Maybe (Tree a)
descendTree Empty x = Just x
descendTree (i :<| is) (Node _ xs) = (descendTree is =<<) . listToMaybe . drop i $ xs

-- | Prune all branches where the predicate is false for every item
-- underneath
pruneForest :: (a -> Bool) -> Forest a -> Forest a
pruneForest p = concatMap go
  where
    go n@(Node x xs)
        | null pf && not (p x) = []
        | otherwise            = [Node x pf]
      where
        pf = concatMap go xs

moveIx :: Tree (Int, a) -> TreeIx -> [((Int, TreeIx), a)]
moveIx t0@(Node _ xs) = \case
    Empty         -> (first . second) Seq.singleton <$> zipWith go [0..] xs
    js@(is :|> i) -> do
      Node (c, y) ys <- maybeToList $ descendTree is t0
      Node (d, _) zs <- maybeToList $ ys !? i
      ((d, is), y) : ((first . second) (js :|>) <$> zipWith go [0..] zs)
  where
    go i n@(Node (d, x) _) = ((d, i), x)

aHeuristic3
    :: Int
    -> AState3
    -> Int
aHeuristic3 allKeys A3{..} = S.size a3Keys - allKeys

aNeighb3
    :: V4 ATree
    -> AState3
    -> Map AState3 Int
aNeighb3 trs A3{..} = M.fromList
    [ (A3 keys' (a3Pos & el e .~ p), c)
    | e <- [ex,ey,ez,ew]
    , let tr = trs ^. el e
    , ((c, p), item) <- moveIx tr (a3Pos ^. el e)
    , case item of
        Just (True, c) -> c `S.member` a3Keys
        _              -> True
    , let keys' = case item of
                    Just (False, c) -> c `S.insert` a3Keys
                    _ -> a3Keys
    ]

day18b :: _ :~> _
day18b = MkSol
    { sParse = Just . parseMaze
    , sShow = show . fst
    -- , sShow = show
    -- , sShow = unlines . map show . snd
    -- , sShow = show . second (map head . group) . (second . map) (foldMap ((:[]) . review _CharFinite . (False,)) . a3Keys)
    -- , sShow = ("\n"++) . Tr.drawTree . fmap (show . (second . fmap) (review _CharFinite)) . view _x
    -- , sShow = foldMap (("\n"++) . Tr.drawTree . fmap (show . (second . fmap) (review _CharFinite)))
    , sSolve = \(displace->(mp, p0s)) ->
        let walls    = M.keysSet . M.filter isNothing $ mp
            labels   = M.mapMaybe (\case Just (IKey c) -> Just (False, c); Just (IDoor c) -> Just (True, c); _ -> Nothing) mp
            keys     = M.mapMaybe (\case Just (IKey c) -> Just c; _ -> Nothing) mp
            allKeys  = S.fromList . toList $ keys
            keyCount = S.size allKeys
            trees = Node (0, Nothing) . flattenMaze walls labels <$> p0s
        in  aStar (aHeuristic3 keyCount)
                  (aNeighb3 trees)
                  (A3 S.empty (pure Seq.empty))
                  (\a -> aHeuristic3 keyCount a == 0)
    }
  where
    pp mp = M.unions [M.singleton p0 Nothing, newWalls, newPlayers, mp]
      where
        (p0,_)  = M.findMin $ M.filter (== Just IChar) mp
        newWalls = M.fromList ((,Nothing) <$> cardinalNeighbs p0)
        newPlayers = M.fromList ((,Just IChar) <$> diagos)
        diagos = map (+p0) [V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1)]
    pr = \case
      '#' -> Just Nothing
      '@' -> Just $ Just IChar
      c -> charFinite c <&> \(up, d) ->
                if up then Just $ IDoor d
                      else Just $ IKey  d
    drawItem = \case
      Nothing -> '#'
      Just IChar -> '@'
      Just (IKey c) -> review _CharFinite (False, c)
      Just (IDoor c) -> review _CharFinite (True, c)

prettyTree :: ATree -> String
prettyTree = Tr.drawTree . fmap (show . (second . fmap) (review _CharFinite))

displace mp = (M.unions [M.singleton p0 Nothing, newWalls, newPlayers, mp], diagos)
  where
    (p0,_)  = M.findMin $ M.filter (== Just IChar) mp
    newWalls = M.fromList (map (,Nothing) $ cardinalNeighbs p0)
    newPlayers = M.fromList (toList . fmap (,Just IChar) $ diagos)
    diagos = (+p0) <$> V4 (V2 1 1) (V2 1 (-1)) (V2 (-1) 1) (V2 (-1) (-1))

parseMaze = parseAsciiMap pr
  where
    pr = \case
      '#' -> Just Nothing
      '@' -> Just $ Just IChar
      c -> charFinite c <&> \(up, d) ->
                if up then Just $ IDoor d
                      else Just $ IKey  d

testTree :: String -> (V4 ATree)
testTree str = Node (0, Nothing) . flattenMaze walls labels <$> p0s
  where
    (mp, p0s) = displace . parseMaze $ str
    walls    = M.keysSet . M.filter isNothing $ mp
    labels   = M.mapMaybe (\case Just (IKey c) -> Just (False, c); Just (IDoor c) -> Just (True, c); _ -> Nothing) mp

test1 = unlines
  [ "###############"
  , "#d.ABC.#.....a#"
  , "######...######"
  , "######.@.######"
  , "######...######"
  , "#b.....#.....c#"
  , "###############"
  ]

-- data Goal = Seek (E (V4 Point)) F26

-- -   Top-left robot collects key `a`.
-- -   Bottom-left robot collects key `b`.
-- -   Top-left robot collects key `c`.
-- -   Bottom-left robot collects key `d`.
-- -   Top-left robot collects key `e`.
-- -   Bottom-left robot collects key `f`.
-- -   Bottom-right robot collects key `g`.
-- -   Top-right robot collects key `h`.
-- -   Bottom-right robot collects key `i`.
-- -   Top-right robot collects key `j`.
-- -   Bottom-right robot collects key `k`.
-- -   Top-right robot collects key `l`.




-- #....f#.......#....m..#.....#.......................#.......#...#.....#.......#.#
-- #######################################.@.#######################################
-- #...#.....#...#...#.#.......#.....#...........#.....#.#.......#...........#.....#
