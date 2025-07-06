{-# LANGUAGE DefaultSignatures #-}

module Hermeneutics.Grammar where

import GHC.Generics

class Grammar g where
    gmap :: (forall i. a i -> b i) -> g a -> g b
    default gmap ::
        (Generic1 g, Grammar (Rep1 g)) => (forall i. a i -> b i) -> g a -> g b
    gmap f = to1 . gmap f . from1

instance Grammar (K1 i c) where
    gmap _ = K1 . unK1

instance Grammar g => Grammar (M1 i c g) where
    gmap f = M1 . gmap f . unM1

instance (Grammar g, Grammar h) => Grammar (g :+: h) where
    gmap f (L1 g) = L1 (gmap f g)
    gmap f (R1 h) = R1 (gmap f h)

instance (Grammar g, Grammar h) => Grammar (g :*: h) where
    gmap f (g :*: h) = gmap f g :*: gmap f h

instance (Functor f, Grammar g) => Grammar (f :.: g) where
    gmap f = Comp1 . fmap (gmap f) . unComp1

instance Grammar g => Grammar (Rec1 g)
