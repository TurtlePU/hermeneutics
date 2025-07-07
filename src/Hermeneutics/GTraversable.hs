{-# LANGUAGE DefaultSignatures #-}

module Hermeneutics.GTraversable where

import GHC.Generics
import Hermeneutics.GFoldable (GFoldable)
import Hermeneutics.Grammar (Grammar)

class (Grammar g, GFoldable g) => GTraversable g where
    gtraverse :: Applicative f => (forall i. a i -> f (b i)) -> g a -> f (g b)
    default gtraverse ::
        (Generic1 g, GTraversable (Rep1 g), Applicative f) =>
        (forall i. a i -> f (b i)) -> g a -> f (g b)
    gtraverse f = fmap to1 . gtraverse f . from1

instance GTraversable (K1 i c) where
    gtraverse _ = pure . K1 . unK1

instance GTraversable g => GTraversable (M1 i c g) where
    gtraverse f = fmap M1 . gtraverse f . unM1

instance (GTraversable g, GTraversable h) => GTraversable (g :+: h) where
    gtraverse f (L1 g) = L1 <$> gtraverse f g
    gtraverse f (R1 h) = R1 <$> gtraverse f h

instance (GTraversable g, GTraversable h) => GTraversable (g :*: h) where
    gtraverse f (g :*: h) = liftA2 (:*:) (gtraverse f g) (gtraverse f h)

instance (Traversable f, GTraversable g) => GTraversable (f :.: g) where
    gtraverse f = fmap Comp1 . traverse (gtraverse f) . unComp1

instance GTraversable g => GTraversable (Rec1 g)
