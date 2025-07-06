{-# LANGUAGE DefaultSignatures #-}

module Hermeneutics.GFoldable where

import GHC.Generics (Generic1, Rep1, from1)

class GFoldable g where
    gfoldMap :: Monoid m => (forall i. a i -> m) -> g a -> m
    default gfoldMap ::
        (Generic1 g, GFoldable (Rep1 g), Monoid m) =>
        (forall i. a i -> m) -> g a -> m
    gfoldMap f = gfoldMap f . from1
