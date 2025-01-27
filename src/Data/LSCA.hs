-- | This module implements the Lowest Single Ancestor Tree (LSAT), as per
-- Fischer 2010
-- (https://www.sciencedirect.com/science/article/abs/pii/S0020019010000487).
--
-- All of the examples below are given over the following graph:
--
-- >   0
-- >  / \
-- > 1   2
-- >  \ / \
-- >   3   4
--
-- which is preprocessed into an 'LSAT' via:
--
-- $setup
-- >>> import qualified Data.Map
-- >>> import qualified Data.Set
-- >>> :{
-- g = mkLSAT $ Data.Map.fromList
--   [ (0, Data.Set.fromList [])
--   , (1, Data.Set.fromList [0])
--   , (2, Data.Set.fromList [0])
--   , (3, Data.Set.fromList [1, 2])
--   , (4, Data.Set.fromList [2])
--   ]
-- :}
module Data.LSCA (
  LSAT,
  mkLSAT,
  lsa,
  lsca,
) where

import qualified Data.LCA.Online as L
import qualified Data.Map as M
import Data.Map (Map)
import Data.Set (Set)
import Data.Foldable (toList)
import Data.Function (fix)
import Data.Maybe (fromJust)



-- | A Lowest Single Ancestor Tree, capable of performing fast 'lsa' and 'lsca'
-- queries.
data LSAT v = LSAT (Map v (L.Path v))


-- | Build an 'LSAT' from a graph represented as a map of edges from nodes to
-- their /parents/. /O(n)/
mkLSAT :: Ord v => Map v (Set v) -> LSAT v
mkLSAT m = LSAT $ fix $ \self -> M.fromList $ do
  -- The algorithm in Fischer 2010 is given in terms of a topological sort and
  -- explicit statefulness. We avoid both here by writing the 'LSAT' as the
  -- least fixpoint of a recursive function, which uses laziness to "sort" the
  -- graph on our behalf.
  (ix, (v, parents)) <- zip [0 ..] $ M.toList m
  case fmap (self M.!) $ toList parents of
    (p : ps) -> do
      -- The LSA of a node is defined as the lowest common ancestor (LCA) of
      -- all of its parents in the tree.
      let l = foldr L.lca p ps
      pure (v, L.cons ix v l)
    [] ->
      -- This case must be apart, since 'L.empty' is a zero with respect to
      -- 'L.lca'.
      pure (v, L.cons ix v L.empty)


-- | Get the lowest single ancestor of a node. /O(log n)/
--
-- ==== __Examples__
--
-- >>> lsa g 0
-- Nothing
--
-- >>> lsa g 1
-- Just 0
--
-- >>> lsa g 2
-- Just 0
--
-- >>> lsa g 3
-- Just 0
--
-- >>> lsa g 4
-- Just 2
lsa :: Ord v => LSAT v -> v -> Maybe v
lsa (LSAT ps) v = do
  -- The LSA of a node is the value at its parent in the tree.
  (_, r, _) <- L.uncons $ L.drop 1 $ ps M.! v
  pure r


-- | Get the lowest single common ancestor between two nodes. /O(log n)/
--
-- ==== __Examples__
--
-- >>> lsca g 1 2
-- 0
--
-- >>> lsca g 1 1
-- 1
--
-- >>> lsca g 3 4
-- 0
lsca :: Ord v => LSAT v -> v -> v -> v
lsca (LSAT ps) v1 v2 = fromJust $ do
  -- The LSCA is the LCA of the two nodes in the tree.
  (_, r, _) <- L.uncons $ L.lca (ps M.! v1) (ps M.! v2)
  pure r

