{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.LCA.Online.Naive
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Naive online calculation of the the lowest common ancestor in /O(h)/
----------------------------------------------------------------------------
module Data.LCA.Online.Naive
  ( Path
  , empty
  , cons
  , uncons
  , view
  , null
  , length
  , isAncestorOf
  , lca
  , keep
  , drop
  , traverseWithKey
  , toList
  , fromList
  , (~=)
  ) where

import Control.Applicative hiding (empty)
import qualified Data.Foldable as F
import Data.LCA.View

#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif

import qualified Prelude
import Prelude hiding
  ( drop
  , length
  , null
  )

-- $setup
-- >>> let length = Data.LCA.Online.Naive.length

-- | An uncompressed 'Path' with memoized length.
data Path a = Path {-# UNPACK #-} !Int [(Int,a)]
  deriving (Show, Read)

-- | Convert a 'Path' to a list of @(ID, value)@ pairs.
toList :: Path a -> [(Int,a)]
toList (Path _ xs) = xs
{-# INLINE toList #-}

-- | Build a 'Path' from a list of @(ID, value)@ pairs.
fromList :: [(Int,a)] -> Path a
fromList xs = Path (Prelude.length xs) xs
{-# INLINE fromList #-}

instance Functor Path where
  fmap f (Path n xs) = Path n [ (k, f a) | (k,a) <- xs]

instance F.Foldable Path where
  foldMap f (Path _ xs) = F.foldMap (f . snd) xs
#if __GLASGOW_HASKELL__ >= 710
  length = length
  null   = null
#endif

-- | /O(1)/ Determine the length of a 'Path'.
length :: Path a -> Int
length (Path n _) = n
{-# INLINE length #-}

-- | /O(1)/ Returns 'True' iff the 'Path' is 'empty'.
null :: Path a -> Bool
null (Path n _) = n == 0
{-# INLINE null #-}

instance Traversable Path where
  traverse f (Path n xs) = Path n <$> traverse (\(k,a) -> (,) k <$> f a) xs

-- | Traverse a 'Path' with access to the node IDs.
traverseWithKey :: Applicative f => (Int -> a -> f b) -> Path a -> f (Path b)
traverseWithKey f (Path n xs) = Path n <$> traverse (\(k,a) -> (,) k <$> f k a) xs
{-# INLINE traverseWithKey #-}

-- | The empty 'Path'
empty :: Path a
empty = Path 0 []

-- | /O(1)/ Invariant: most operations assume that the keys @k@ are globally unique
--
-- Extend the path with a new node ID and value.
cons :: Int -> a -> Path a -> Path a
cons k a (Path n xs) = Path (n + 1) $ (k,a):xs
{-# INLINE cons #-}

-- | /O(1)/ Extract the node ID and value from the newest node on the 'Path'.
uncons :: Path a -> Maybe (Int, a, Path a)
uncons (Path _ []) = Nothing
uncons (Path n ((k,a):xs)) = Just (k,a,Path (n - 1) xs)
{-# INLINE uncons #-}

-- | /O(1)/ Extract the node ID and value from the newest node on the 'Path', slightly faster than 'uncons'.
view :: Path a -> View Path a
view (Path _ []) = Root
view (Path n ((k,a):xs)) = Node k a (Path (n - 1) xs)
{-# INLINE view #-}

-- | /O(h - k)/ to @'keep' k@ elements of 'Path' of 'length' @h@
keep :: Int -> Path a -> Path a
keep k p@(Path n xs)
  | k >= n    = p
  | otherwise = Path k $ Prelude.drop (n - k) xs
{-# INLINE keep #-}

-- | /O(k)/ to @'drop' k@ elements from a 'Path'
drop :: Int -> Path a -> Path a
drop k (Path n xs)
  | k >= n    = empty
  | otherwise = Path (n - k) (Prelude.drop k xs)
{-# INLINE drop #-}

-- | /O(h)/ @xs `isAncestorOf` ys@ holds when @xs@ is a prefix starting at the root of 'Path' @ys@.
isAncestorOf :: Path a -> Path b -> Bool
isAncestorOf xs ys = xs ~= keep (length xs) ys
{-# INLINE isAncestorOf #-}

infix 4 ~=
-- | /O(1)/ Compare to see if two trees have the same root key
(~=) :: Path a -> Path b -> Bool
Path _ []        ~= Path _ []        = True
Path _ ((i,_):_) ~= Path _ ((j,_):_) = i == j
_                ~= _                = False
{-# INLINE (~=) #-}

-- | /O(h)/ Compute the lowest common ancestor of two paths
--
-- >>> let fromList' = fromList . map (flip (,) ())
-- >>> length (lca (fromList' [1, 2, 3, 4, 5, 6]) (fromList' [7, 8, 3, 4, 5, 6]))
-- 4
lca :: Path a -> Path b -> Path a
lca xs0 ys0 = case compare nxs nys of
    LT -> go nxs (toList xs0) (toList (keep nxs ys0))
    EQ -> go nxs (toList xs0) (toList ys0)
    GT -> go nys (toList (keep nys xs0)) (toList ys0)
  where
    nxs = length xs0
    nys = length ys0
    go k xss@((i,_):xs) ((j,_):ys)
      | i == j    = Path k xss
      | otherwise = go (k - 1) xs ys
    go _ _ _ = empty
{-# INLINE lca #-}
