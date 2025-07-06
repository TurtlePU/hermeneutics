{-# LANGUAGE DefaultSignatures #-}

module Hermeneutics.GTraversable where

import GHC.Generics           (Generic1, Rep1, from1, to1)
import Hermeneutics.GFoldable (GFoldable)
import Hermeneutics.Grammar   (Grammar)

class (Grammar g, GFoldable g) => GTraversable g where
    gtraverse :: Applicative f => (forall i. a i -> f (b i)) -> g a -> f (g b)
    default gtraverse ::
        (Generic1 g, GTraversable (Rep1 g), Applicative f) =>
        (forall i. a i -> f (b i)) -> g a -> f (g b)
    gtraverse f = fmap to1 . gtraverse f . from1
