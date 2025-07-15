{-# LANGUAGE QuantifiedConstraints #-}

module Hermeneutics.ManySorted.Tree where

import Hermeneutics.Bound1 (Bound1, Monad1 (..), lift1)
import Hermeneutics.Functor1 (Functor1 (..))
import Hermeneutics.Grammar (Grammar, gmap)

data Tree t g v s = Leaf (v s) | Node (g s (t (Tree t g) v))

instance (Bound1 t, forall s. Grammar (g s)) => Functor1 (Tree t g) where
    fmap1 f = bind1 (unit1 . f)

instance (Bound1 t, forall s. Grammar (g s)) => Monad1 (Tree t g) where
    unit1 = Leaf
    bind1 f (Leaf u) = f u
    bind1 f (Node n) = Node (gmap (lift1 f) n)
