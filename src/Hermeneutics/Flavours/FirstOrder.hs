module Hermeneutics.Flavours.FirstOrder where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Flavours (HFunctor (..))

newtype FirstOrder f v s = FstOrder { fstOrder :: f (v (s :| '[])) }

instance Functor f => HFunctor (FirstOrder f) where
    hmap f = FstOrder . fmap f . fstOrder
