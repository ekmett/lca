module Data.LCA.View (View(..)) where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid

data View f a
  = Root
  | Node {-# UNPACK #-} !Int a (f a)
  deriving (Eq,Ord,Read,Show)

instance Functor f => Functor (View f) where
  fmap _ Root = Root
  fmap f (Node k a as) = Node k (f a) (fmap f as)

instance Foldable f => Foldable (View f) where
  foldMap _ Root = mempty
  foldMap f (Node _ a as) = f a <> foldMap f as

instance Traversable f => Traversable (View f) where
  traverse _ Root = pure Root
  traverse f (Node k a as) = Node k <$> f a <*> traverse f as
