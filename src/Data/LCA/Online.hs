-----------------------------------------------------------------------------
-- |
-- Module      :  Data.LCA.Online
-- Copyright   :  (C) 2011-2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides online calculation of the the lowest common ancestor in /O(log h)/
-- by compressing the spine of the paths using a skew binary random access
-- list.
--
-- Algorithms used here assume that the key values chosen for @k@ are
-- globally unique.
--
----------------------------------------------------------------------------
module Data.LCA.Online
  ( Path
  , empty
  , toList
  , cons
  , null
  , length
  , isAncestorOf
  , lca
  , keep
  , drop
  , traverseWithKey
  , (~=)
  ) where

import Control.Applicative hiding (empty)
import Data.Foldable hiding (toList)
import Data.Traversable
import Data.Monoid
import Prelude hiding (length, null, drop)

-- | Compressed paths using skew binary random access lists
data Path k a
  = Nil
  | Cons {-# UNPACK #-} !Int -- ^ the number of elements @n@ in this entire skew list
         {-# UNPACK #-} !Int -- ^ the number of elements @w@ in this binary tree node
         (Tree k a)          -- ^ a complete binary tree @t@ of with @w@ elements
         (Path k a)          -- ^ @n - w@ elements in a linked list @ts@, of complete trees in ascending order by size
  deriving (Show, Read)

instance Functor (Path k) where
  fmap _ Nil = Nil
  fmap f (Cons n k t ts) = Cons n k (fmap f t) (fmap f ts)

instance Foldable (Path k) where
  foldMap _ Nil = mempty
  foldMap f (Cons _ _ t ts) = foldMap f t `mappend` foldMap f ts

instance Traversable (Path k) where
  traverse _ Nil = pure Nil
  traverse f (Cons n k t ts) = Cons n k <$> traverse f t <*> traverse f ts

-- | Complete binary trees
-- NB: we could ensure the complete tree invariant
data Tree k a
  = Bin k a (Tree k a) (Tree k a)
  | Tip k a
  deriving (Show, Read)

instance Functor (Tree k) where
  fmap f (Bin n a l r) = Bin n (f a) (fmap f l) (fmap f r)
  fmap f (Tip n a)     = Tip n (f a)

instance Foldable (Tree k) where
  foldMap f (Bin _ a l r) = f a `mappend` foldMap f l `mappend` foldMap f r
  foldMap f (Tip _ a)     = f a

instance Traversable (Tree k) where
  traverse f (Bin n a l r) = Bin n <$> f a <*> traverse f l <*> traverse f r
  traverse f (Tip n a) = Tip n <$> f a

toList :: Path k a -> [(k,a)]
toList Nil = []
toList (Cons _ _ t ts) = go t (toList ts) where
  go (Tip k a) xs     = (k,a) : xs
  go (Bin k a l r) xs = (k,a) : go l (go r xs)

traverseWithKey :: Applicative f => (k -> a -> f b) -> Path k a -> f (Path k b)
traverseWithKey _ Nil = pure Nil
traverseWithKey f (Cons n k t ts) = Cons n k <$> traverseTreeWithKey f t <*> traverseWithKey f ts

-- | The empty path
empty :: Path k a
empty = Nil

-- | /O(1)/
length :: Path k a -> Int
length Nil = 0
length (Cons n _ _ _) = n

-- | /O(1)/
null :: Path k a -> Bool
null Nil = True
null _ = False

-- | /O(1)/ Invariant: most operations assume that the keys @k@ are globally unique
cons :: k -> a -> Path k a -> Path k a
cons k a (Cons n w t (Cons _ w' t2 ts)) | w == w' = Cons (n + 1) (2 * w + 1) (Bin k a t t2) ts
cons k a ts = Cons (length ts + 1) 1 (Tip k a) ts

-- | /O(log (h - k))/ to @keep k@ elements of path of height @h@
keep :: Int -> Path k a -> Path k a
keep _ Nil = Nil
keep k xs@(Cons n w t ts)
  | k >= n    = xs
  | otherwise = case compare k (n - w) of
    GT -> keepT (k - n + w) w t ts
    EQ -> ts
    LT -> keep k ts

-- | /O(log k)/ to @drop k@ elements from a path
drop :: Int -> Path k a -> Path k a
drop k xs = keep (length xs - k) xs

-- | /O(log h)/ Compute the lowest common ancestor
lca :: Eq k => Path k a -> Path k b -> Path k a
lca xs ys = case compare nxs nys of
    LT -> lca' xs (keep nxs ys)
    EQ -> lca' xs ys
    GT -> lca' (keep nys xs) ys
  where
    nxs = length xs
    nys = length ys

-- /O(log h)/ @xs `isAncestorOf` ys@ holds when @xs@ is a prefix starting at the root of path @ys@.
isAncestorOf :: Eq k => Path k a -> Path k b -> Bool
isAncestorOf xs ys = xs ~= keep (length xs) ys

infix 4 ~=
-- | /O(1)/ Compare to see if two trees have the same leaf key
(~=) :: Eq k => Path k a -> Path k b -> Bool
Nil          ~= Nil          = True
Cons _ _ s _ ~= Cons _ _ t _ = sameT s t
_            ~= _            = False

-- * Utilities
consT :: Int -> Tree k a -> Path k a -> Path k a
consT w t ts = Cons (w + length ts) w t ts

keepT :: Int -> Int -> Tree k a -> Path k a -> Path k a
keepT n w (Bin _ _ l r) ts = case compare n w2 of
  LT              -> keepT n w2 r ts
  EQ              -> consT w2 r ts
  GT | n == w - 1 -> consT w2 l (consT w2 r ts)
     | otherwise  -> keepT (n - w2) w2 l (consT w2 r ts)
  where w2 = div w 2
keepT _ _ _ ts = ts

sameT :: Eq k => Tree k a -> Tree k b -> Bool
sameT xs ys = root xs == root ys

-- | invariant: both paths have the same number of elements and the same shape
lca' :: Eq k => Path k a -> Path k b -> Path k a
lca' h@(Cons _ w x xs) (Cons _ _ y ys)
  | sameT x y = h
  | xs ~= ys  = lcaT w x y xs
  | otherwise = lca' xs ys
lca' _ _ = Nil

lcaT :: Eq k => Int -> Tree k a -> Tree k b -> Path k a -> Path k a
lcaT w (Bin _ _ la ra) (Bin _ _ lb rb) ts
  | sameT la lb = consT w2 la (consT w2 ra ts)
  | sameT ra rb = lcaT w2 la lb (consT w ra ts)
  | otherwise   = lcaT w2 ra rb ts
  where w2 = div w 2
lcaT _ _ _ ts = ts

traverseTreeWithKey :: Applicative f => (k -> a -> f b) -> Tree k a -> f (Tree k b)
traverseTreeWithKey f (Bin k a l r) = Bin k <$> f k a <*> traverseTreeWithKey f l <*> traverseTreeWithKey f r
traverseTreeWithKey f (Tip k a)     = Tip k <$> f k a

-- | /O(1)/
root :: Tree k a -> k
root (Tip k _)     = k
root (Bin k _ _ _) = k
