{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}


module AOC.Common.Subset (
    findSubset
  , testFinder
  , allBranches
  , buildDTree
  , renderBranches
  , renderBranchesChar
  ) where

import           AOC.Util
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer hiding       (First(..))
import           Data.Bitraversable
import           Data.Foldable
import           Data.Functor
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.List.NonEmpty                (NonEmpty(..))
import           Data.Map                          (Map)
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.Set                          (Set)
import qualified Data.Graph.Inductive.Graph        as G
import qualified Data.GraphViz                     as GV
import qualified Data.GraphViz.Printing            as GV
import qualified Data.List.NonEmpty                as NE
import qualified Data.Map                          as M
import qualified Data.Set                          as S
import qualified Data.Set.NonEmpty                 as NES
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL

attrEntropy :: Ord a => Set (Set a) -> Set a -> Double
attrEntropy xs x = pLT * hLT + pGT * hGT
  where
    ltWeight = 2 * lt + unknown
    gtWeight = 2 * gt + unknown
    pLT      = ltWeight / (fromIntegral (S.size xs) * 2)
    pGT      = gtWeight / (fromIntegral (S.size xs) * 2)
    hLT      = lt * entroRecip (ltWeight/2) + unknown * entroRecip ltWeight
    hGT      = gt * entroRecip (gtWeight/2) + unknown * entroRecip gtWeight
    (Sum lt, Sum gt, Sum _, Sum unknown) = flip foldMap xs $ \y ->
      if | y == x                   -> (mempty, mempty, Sum (1 :: Int), mempty)
         | x `S.isProperSubsetOf` y -> (Sum 1 , mempty, mempty, mempty)
         | y `S.isProperSubsetOf` x -> (mempty, Sum 1 , mempty, mempty)
         | otherwise                -> (mempty, mempty, mempty, Sum 1 )

filterTest :: Ord a => Set (Set a) -> Set a -> Ordering -> Set (Set a)
filterTest xs x = \case
    LT -> flip S.filter xs $ \y -> not $ y `S.isSubsetOf` x
    EQ -> S.singleton x
    GT -> flip S.filter xs $ \y -> not $ x `S.isSubsetOf` y

findSubset
    :: (Monad m, Ord a)
    => (Set a -> m Ordering)        -- ^ tester
    -> Bool                         -- ^ whether or not to include empty set and full set
    -> Set a                        -- ^ full set of items
    -> m (Maybe (Set a))            -- ^ subset that matches tester
findSubset tester includeEdge x0 = runMaybeT . go . ruleOut . S.powerSet $ x0
  where
    ruleOut 
      | includeEdge = id
      | otherwise   = S.filter (`notElem` [S.empty, x0])
    go xs = do
        (subset, _) <- maybeAlt $
            minimumBy (comparing snd) <$> NE.nonEmpty entropies
        s0@(NES.IsNonEmpty rest) <- filterTest xs subset <$> lift (tester subset)
        let res :| others = NES.toList rest
        if null others
          then pure res
          else go s0
      where
        entropies = M.toList $ M.fromSet (attrEntropy xs) xs

entroRecip :: Double -> Double
entroRecip 0 = 0
entroRecip p = -(1/p) * log (1/p)


-- | Get the number of guesses needed for each possible subset, for
-- n items.
testFinder
    :: Bool         -- ^ whether or not to include empty set and full set
    -> Int
    -> Map (Set Int) Int
testFinder incl n = M.fromSet (\x -> getSum . execWriter $ findSubset (go x) incl xs) $ S.powerSet xs
  where
    xs  = S.fromList [0 .. n - 1]
    go goal x = compare (sumSet x) goalAmt <$ tell (Sum 1)
      where
        goalAmt = sumSet goal
    sumSet :: Set Int -> Int
    sumSet = getSum . foldMap (Sum . (2 ^))


data DTree a = DNode { dTest :: Set a
                     , dLT   :: Maybe (DTree a)
                     , dGT   :: Maybe (DTree a)
                     }
  deriving (Show)
makeBaseFunctor ''DTree

renderBranches :: (Ord a, Show a) => Bool -> Set a -> Maybe TL.Text
renderBranches incl = fmap (printGraph (show . toList) . dTreeGraph) . buildDTree incl

renderBranchesChar :: Bool -> Set Char -> Maybe TL.Text
renderBranchesChar incl = fmap (printGraph toList . dTreeGraph) . buildDTree incl

buildDTree :: Ord a => Bool -> Set a -> Maybe (DTree a)
buildDTree incl xs = do
    bs <- (traverse . traverse) NE.nonEmpty . allBranches incl $ xs
    branchesToDTree <$> NE.nonEmpty bs

allBranches :: Ord a => Bool -> Set a -> [(Set a, [(Set a, Ordering)])]
allBranches incl = mapMaybe (bitraverse id pure) . runWriterT . findSubset branchOut incl
  where
    branchOut x = asum [ LT <$ tell [(x, LT)]
                       , EQ <$ tell [(x, EQ)]
                       , GT <$ tell [(x, GT)]
                       ]

branchesToDTree
    :: forall a. ()
    => NonEmpty (Set a, NonEmpty (Set a, Ordering))
    -> DTree a
branchesToDTree = apo go
  where
    go  :: NonEmpty (Set a, NonEmpty (Set a, Ordering))
        -> DTreeF a (Either (DTree a) (NonEmpty (Set a, NonEmpty (Set a, Ordering))))
    go xs@((_, (t, _) :| _) :| _) = DNodeF t (uncurry reshape lt) (uncurry reshape gt)
      where
        lt = flip foldMap (toList xs) $ \case
                (r, (_, LT) :| (y:ys)) -> (mempty                                , [(r, y :| ys)])
                (r, (_, LT) :| []    ) -> (Just (First (DNode r Nothing Nothing)), mempty        )
                _                      -> mempty
        gt = flip foldMap (toList xs) $ \case
                (r, (_, GT) :| (y:ys)) -> (mempty                                , [(r, y :| ys)])
                (r, (_, GT) :| []    ) -> (Just (First (DNode r Nothing Nothing)), mempty        )
                _                      -> mempty
        reshape = \case
          Just (First x) -> \_ -> Just $ Left x
          Nothing        -> fmap Right . NE.nonEmpty

printGraph :: forall a. Show a => (Set a -> String) -> Gr (Set a) Bool -> TL.Text
printGraph f = GV.printIt . GV.graphToDot params
  where
    params :: GV.GraphvizParams G.Node (Set a) Bool _ _
    params = GV.nonClusteredParams
      { GV.fmtNode = \(_, xs) -> [GV.toLabel (f xs)]
      , GV.fmtEdge = \(_, _, b) -> [GV.toLabel $ if b then "GT" else "LT"]
      }


dTreeGraph :: forall a. DTree a -> Gr (Set a) Bool
dTreeGraph = flip evalState 0 . cata go
  where
    go :: DTreeF a (State Int (Gr (Set a) Bool))
       -> State Int (Gr (Set a) Bool)
    go (DNodeF x lt gt) = do
      n   <- fresh
      lt' <- sequence lt
      gt' <- sequence gt
      let ltRoot = lt' <&> \g ->
            let (r, _) = G.nodeRange g
            in  G.insEdge (n, r, False)
          gtRoot = gt' <&> \g ->
            let (r, _) = G.nodeRange g
            in  G.insEdge (n, r, True)
      pure $ fromMaybe id ltRoot
           . fromMaybe id gtRoot
           . G.insNode (n, x)
           $ foldr (G.ufold (G.&)) G.empty (catMaybes [lt', gt'])

fresh :: State Int Int
fresh = state $ \i -> (i, i + 1)
