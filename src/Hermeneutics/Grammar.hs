{-# LANGUAGE DefaultSignatures #-}

module Hermeneutics.Grammar where

import GHC.Generics (Generic1, Rep1, from1, to1)

class Grammar g where
    gmap :: (forall i. a i -> b i) -> g a -> g b
    default gmap ::
        (Generic1 g, Grammar (Rep1 g)) => (forall i. a i -> b i) -> g a -> g b
    gmap f = to1 . gmap f . from1
