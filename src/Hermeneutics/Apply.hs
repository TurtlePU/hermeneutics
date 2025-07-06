module Hermeneutics.Apply where

import Hermeneutics.GFoldable    (GFoldable (..))
import Hermeneutics.Grammar      (Grammar (..))
import Hermeneutics.GTraversable (GTraversable (..))

newtype (:&:) x f = Apply { runApply :: f x }

instance Grammar ((:&:) x) where gmap f = Apply . f . runApply
instance GFoldable ((:&:) x) where gfoldMap f = f . runApply
instance GTraversable ((:&:) x) where gtraverse f = fmap Apply . f . runApply
