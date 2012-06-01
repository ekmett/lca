-----------------------------------------------------------------------------
-- |
-- Module      :  Data.LCA.Online.Naive
-- Copyright   :  (C) 2011-2012 Edward Kmett
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
import Data.Foldable hiding (toList)
import Data.Traversable
import Prelude hiding (length, null, drop)
import qualified Prelude

data Path a = Path {-# UNPACK #-} !Int [(Int,a)]
  deriving (Show, Read)

toList :: Path a -> [(Int,a)]
toList (Path _ xs) = xs

fromList :: [(Int,a)] -> Path a
fromList xs = Path (Prelude.length xs) xs

instance Functor Path where
  fmap f (Path n xs) = Path n [ (k, f a) | (k,a) <- xs]

instance Foldable Path where
  foldMap f (Path _ xs) = foldMap (f . snd) xs

instance Traversable Path where
  traverse f (Path n xs) = Path n <$> traverse (\(k,a) -> (,) k <$> f a) xs

traverseWithKey :: Applicative f => (Int -> a -> f b) -> Path a -> f (Path b)
traverseWithKey f (Path n xs) = Path n <$> traverse (\(k,a) -> (,) k <$> f k a) xs

-- | The empty path
empty :: Path a
empty = Path 0 []

-- | /O(1)/
length :: Path a -> Int
length (Path n _) = n

-- | /O(1)/
null :: Path a -> Bool
null (Path n _) = n == 0

-- | /O(1)/ Invariant: most operations assume that the keys @k@ are globally unique
cons :: Int -> a -> Path a -> Path a
cons k a (Path n xs) = Path (n + 1) $ (k,a):xs

-- | /O(h - k)/ to @keep k@ elements of path of height @h@
keep :: Int -> Path a -> Path a
keep k p@(Path n xs)
  | k >= n    = p
  | otherwise = Path k $ Prelude.drop (n - k) xs

-- | /O(k)/ to @drop k@ elements from a path
drop :: Int -> Path a -> Path a
drop k (Path n xs)
  | k >= n    = empty
  | otherwise = Path (n - k) (Prelude.drop k xs)

-- | /O(h)/ Compute the lowest common ancestor
lca :: Path a -> Path b -> Path a
lca xs ys = case compare nxs nys of
    LT -> lca' nxs (toList xs) (toList (keep nxs ys))
    EQ -> lca' nxs (toList xs) (toList ys)
    GT -> lca' nys (toList (keep nys xs)) (toList ys)
  where
    nxs = length xs
    nys = length ys

-- | invariant: both paths have the same number of elements
lca' :: Int -> [(Int,a)] -> [(Int,b)] -> Path a
lca' k xss@((i,_):xs) ((j,_):ys)
  | i == j    = Path k xss
  | otherwise = lca' (k - 1) xs ys
lca' _ _ _ = empty

-- /O(h)/ @xs `isAncestorOf` ys@ holds when @xs@ is a prefix starting at the root of path @ys@.
isAncestorOf :: Path a -> Path b -> Bool
isAncestorOf xs ys = xs ~= keep (length xs) ys

infix 4 ~=
-- | /O(1)/ Compare to see if two trees have the same leaf key
(~=) :: Path a -> Path b -> Bool
Path _ []        ~= Path _ []        = True
Path _ ((i,_):_) ~= Path _ ((j,_):_) = i == j
_                ~= _                = False


