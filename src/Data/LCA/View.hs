{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.LCA.View
-- Copyright   :  (C) 2012-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Data.LCA.View (View(..)) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid
#endif

-- | Provides a consistent 'View' for peeling off the bottom node of a path.
data View f a
  = Root
  | Node {-# UNPACK #-} !Int a (f a)
  deriving (Eq,Ord,Read,Show)

instance Functor f => Functor (View f) where
  fmap _ Root = Root
  fmap f (Node k a as) = Node k (f a) (fmap f as)

instance Foldable f => Foldable (View f) where
  foldMap _ Root = mempty
  foldMap f (Node _ a as) = f a `mappend` foldMap f as

instance Traversable f => Traversable (View f) where
  traverse _ Root = pure Root
  traverse f (Node k a as) = Node k <$> f a <*> traverse f as
