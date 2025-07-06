{-# LANGUAGE DefaultSignatures #-}

module Hermeneutics.GFoldable where

import GHC.Generics

class GFoldable g where
    gfoldMap :: Monoid m => (forall i. a i -> m) -> g a -> m
    default gfoldMap ::
        (Generic1 g, GFoldable (Rep1 g), Monoid m) =>
        (forall i. a i -> m) -> g a -> m
    gfoldMap f = gfoldMap f . from1

instance GFoldable (K1 i c) where
    gfoldMap _ _ = mempty

instance GFoldable g => GFoldable (M1 i c g) where
    gfoldMap f = gfoldMap f . unM1

instance (GFoldable g, GFoldable h) => GFoldable (g :+: h) where
    gfoldMap f (L1 g) = gfoldMap f g
    gfoldMap f (R1 h) = gfoldMap f h

instance (GFoldable g, GFoldable h) => GFoldable (g :*: h) where
    gfoldMap f (g :*: h) = gfoldMap f g <> gfoldMap f h

instance (Foldable f, GFoldable g) => GFoldable (f :.: g) where
    gfoldMap f = foldMap (gfoldMap f) . unComp1

instance GFoldable g => GFoldable (Rec1 g)
