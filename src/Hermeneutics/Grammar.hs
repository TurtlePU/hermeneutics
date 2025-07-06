{-# LANGUAGE DefaultSignatures #-}

module Hermeneutics.Grammar where

import GHC.Generics (Generic1, Rep1, from1, to1)

class Grammar g where
    gmap :: (forall i. a i -> b i) -> g a -> g b
    default gmap ::
        (Generic1 g, Grammar (Rep1 g)) => (forall i. a i -> b i) -> g a -> g b
    gmap f = to1 . gmap f . from1

class GFoldable g where
    gfoldMap :: Monoid m => (forall i. a i -> m) -> g a -> m
    default gfoldMap ::
        (Generic1 g, GFoldable (Rep1 g), Monoid m) =>
        (forall i. a i -> m) -> g a -> m
    gfoldMap f = gfoldMap f . from1

class (Grammar g, GFoldable g) => GTraversable g where
    gtraverse :: Applicative f => (forall i. a i -> f (b i)) -> g a -> f (g b)
    default gtraverse ::
        (Generic1 g, GTraversable (Rep1 g), Applicative f) =>
        (forall i. a i -> f (b i)) -> g a -> f (g b)
    gtraverse f = fmap to1 . gtraverse f . from1
