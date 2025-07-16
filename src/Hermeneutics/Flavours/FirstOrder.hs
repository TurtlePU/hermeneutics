module Hermeneutics.Flavours.FirstOrder where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Flavours (HFoldable (..), HFunctor (..), HTraversable (..))

newtype FirstOrder f v s = FstOrder { fstOrder :: f (v (s :| '[])) }

instance Functor f => HFunctor (FirstOrder f) where
    hmap f = FstOrder . fmap f . fstOrder

instance Foldable f => HFoldable (FirstOrder f) where
    hfoldMap f = foldMap f . fstOrder

instance Traversable f => HTraversable (FirstOrder f) where
    htraverse f = fmap FstOrder . traverse f . fstOrder
