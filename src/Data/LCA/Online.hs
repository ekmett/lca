{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.LCA.Online
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides online calculation of the the lowest common ancestor in /O(log h)/
-- by compressing the spine of a 'Path' using a skew-binary random access
-- list.
--
-- This library implements the technique described in my talk
--
-- <http://www.slideshare.net/ekmett/skewbinary-online-lowest-common-ancestor-search>
--
-- to improve the known asymptotic bounds on both online lowest common ancestor search
--
-- <http://en.wikipedia.org/wiki/Lowest_common_ancestor>
--
-- and the online level ancestor problem:
--
-- <http://en.wikipedia.org/wiki/Level_ancestor_problem>
--
-- Algorithms used here assume that the key values chosen for @k@ are
-- globally unique.
----------------------------------------------------------------------------
module Data.LCA.Online
  ( Path
  , lca
  , empty
  , cons
  , uncons, view
  , null
  , length
  , isAncestorOf
  , keep
  , drop
  , traverseWithKey
  , toList
  , fromList
  , (~=)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding (empty)
import Data.Foldable hiding (toList)
import Data.Traversable
import Data.Monoid
#endif

import Data.LCA.View

import Prelude hiding
  ( drop
#if __GLASGOW_HASKELL__ < 710
  , length
  , null
#endif
  )

-- | Complete binary trees
data Tree a
  = Bin {-# UNPACK #-} !Int a (Tree a) (Tree a)
  | Tip {-# UNPACK #-} !Int a
  deriving (Show, Read)

instance Functor Tree where
  fmap f (Bin n a l r) = Bin n (f a) (fmap f l) (fmap f r)
  fmap f (Tip n a)     = Tip n (f a)

instance Foldable Tree where
  foldMap f (Bin _ a l r) = f a `mappend` foldMap f l `mappend` foldMap f r
  foldMap f (Tip _ a)     = f a

instance Traversable Tree where
  traverse f (Bin n a l r) = Bin n <$> f a <*> traverse f l <*> traverse f r
  traverse f (Tip n a)     = Tip n <$> f a

sameT :: Tree a -> Tree b -> Bool
sameT xs ys = root xs == root ys where
  root (Tip k _)     = k
  root (Bin k _ _ _) = k
{-# INLINE sameT #-}

-- | Compressed paths using skew binary random access lists
data Path a
  = Nil
  | Cons {-# UNPACK #-} !Int -- the number of elements @n@ in this entire skew list
         {-# UNPACK #-} !Int -- the number of elements @w@ in this binary tree node
         (Tree a)            -- a complete binary tree @t@ of with @w@ elements
         (Path a)            -- @n - w@ elements in a linked list @ts@, of complete trees in ascending order by size
  deriving (Show, Read)

instance Functor Path where
  fmap _ Nil = Nil
  fmap f (Cons n k t ts) = Cons n k (fmap f t) (fmap f ts)

instance Foldable Path where
  foldMap _ Nil = mempty
  foldMap f (Cons _ _ t ts) = foldMap f t `mappend` foldMap f ts
#if __GLASGOW_HASKELL__ >= 710
  length Nil = 0
  length (Cons n _ _ _) = n
  {-# INLINE length #-}

  null Nil = True
  null _ = False
  {-# INLINE null #-}

#else

-- | /O(1)/ Determine the 'length' of a 'Path'.
length :: Path a -> Int
length Nil = 0
length (Cons n _ _ _) = n
{-# INLINE length #-}

-- | /O(1)/ Returns 'True' iff the path is 'empty'.
null :: Path a -> Bool
null Nil = True
null _ = False
{-# INLINE null #-}

#endif

instance Traversable Path where
  traverse _ Nil = pure Nil
  traverse f (Cons n k t ts) = Cons n k <$> traverse f t <*> traverse f ts

consT :: Int -> Tree a -> Path a -> Path a
consT w t ts = Cons (w + length ts) w t ts
{-# INLINE consT #-}

-- | Convert a 'Path' to a list of @(ID, value)@ pairs.
toList :: Path a -> [(Int,a)]
toList Nil = []
toList (Cons _ _ t ts) = go t (toList ts) where
  go (Tip k a) xs     = (k,a) : xs
  go (Bin k a l r) xs = (k,a) : go l (go r xs)

-- | Build a 'Path' from a list of @(ID, value)@ pairs.
fromList :: [(Int,a)] -> Path a
fromList [] = Nil
fromList ((k,a):xs) = cons k a (fromList xs)

-- | Traverse a 'Path' with access to the node IDs.
traverseWithKey :: Applicative f => (Int -> a -> f b) -> Path a -> f (Path b)
traverseWithKey f = go where
  go Nil = pure Nil
  go (Cons n k t ts) = Cons n k <$> goT t <*> go ts
  goT (Bin k a l r) = Bin k <$> f k a <*> goT l <*> goT r
  goT (Tip k a)     = Tip k <$> f k a
{-# INLINE traverseWithKey #-}

-- | The 'empty' 'Path'
empty :: Path a
empty = Nil
{-# INLINE empty #-}

-- | /O(1)/ Invariant: most operations assume that the keys @k@ are globally unique
--
-- Extend the 'Path' with a new node ID and value.
cons :: Int -> a -> Path a -> Path a
cons k a (Cons n w t (Cons _ w' t2 ts)) | w == w' = Cons (n + 1) (2 * w + 1) (Bin k a t t2) ts
cons k a ts = Cons (length ts + 1) 1 (Tip k a) ts
{-# INLINE cons #-}

-- | /O(1)/ Extract the node ID and value from the newest node on the 'Path'.
uncons :: Path a -> Maybe (Int, a, Path a)
uncons Nil = Nothing
uncons (Cons _ _ (Tip k a) ts) = Just (k, a, ts)
uncons (Cons _ w (Bin k a l r) ts) = Just (k, a, consT w2 l (consT w2 r ts)) where w2 = div w 2
{-# INLINE uncons #-}

-- | /O(1)/ Extract the node ID and value from the newest node on the 'Path', slightly faster than 'uncons'.
view :: Path a -> View Path a
view Nil = Root
view (Cons _ _ (Tip k a) ts) = Node k a ts
view (Cons _ w (Bin k a l r) ts) = Node k a (consT w2 l (consT w2 r ts)) where w2 = div w 2
{-# INLINE view #-}

-- | /O(log (h - k))/ to @'keep' k@ elements of 'Path' of 'length' @h@
--
-- This solves the online version of the \"level ancestor problem\" with no preprocessing in /O(log h)/ time,
-- improving known complexity bounds.
--
-- <http://en.wikipedia.org/wiki/Level_ancestor_problem>
keep :: Int -> Path a -> Path a
keep = go where
  go _ Nil = Nil
  go k xs@(Cons n w t ts)
    | k >= n    = xs
    | otherwise = case compare k (n - w) of
      GT -> goT (k - n + w) w t ts
      EQ -> ts
      LT -> go k ts
  goT n w (Bin _ _ l r) ts = case compare n w2 of
    LT              -> goT n w2 r ts
    EQ              -> consT w2 r ts
    GT | n == w - 1 -> consT w2 l (consT w2 r ts)
       | otherwise  -> goT (n - w2) w2 l (consT w2 r ts)
    where w2 = div w 2
  goT _ _ _ ts = ts
{-# INLINE keep #-}

-- | /O(log k)/ to @'drop' k@ elements from a 'Path'
drop :: Int -> Path a -> Path a
drop k xs = keep (length xs - k) xs
{-# INLINE drop #-}

-- | /O(log h)/ @xs `isAncestorOf` ys@ holds when @xs@ is a prefix starting at the root of 'Path' @ys@.
isAncestorOf :: Path a -> Path b -> Bool
isAncestorOf xs ys = xs ~= keep (length xs) ys
{-# INLINE isAncestorOf #-}

infix 4 ~=
-- | /O(1)/ Compare to see if two trees have the same root key
(~=) :: Path a -> Path b -> Bool
Nil          ~= Nil          = True
Cons _ _ s _ ~= Cons _ _ t _ = sameT s t
_            ~= _            = False
{-# INLINE (~=) #-}

-- $doctest
-- >>> let fromList' = fromList . map (flip (,) ())
-- >>> length (lca (fromList' [1, 2, 3, 4, 5, 6]) (fromList' [7, 8, 3, 4, 5, 6]))
-- 4

-- | /O(log h)/ Compute the lowest common ancestor of two paths.
lca :: Path a -> Path b -> Path a
lca xs0 ys0 = case compare nxs nys of
    LT -> go xs0 (keep nxs ys0)
    EQ -> go xs0 ys0
    GT -> go (keep nys xs0) ys0
  where
    nxs = length xs0
    nys = length ys0
    go h@(Cons _ w x xs) (Cons _ _ y ys)
      | sameT x y = h
      | xs ~= ys  = goT w x y xs
      | otherwise = go xs ys
    go _ _ = Nil

    goT w (Bin _ _ la ra) (Bin _ _ lb rb) ts
      | sameT la lb = consT w2 la (consT w2 ra ts)
      | sameT ra rb = goT w2 la lb (consT w2 ra ts)
      | otherwise   = goT w2 ra rb ts
      where w2 = div w 2
    goT _ _ _ ts = ts
{-# INLINE lca #-}
