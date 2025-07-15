module Hermeneutics.Functor1 where

import GHC.Generics ((:.:) (..))

type f ~> g = forall a. f a -> g a

class Functor1 t where
    fmap1 :: (f ~> g) -> (t f ~> t g)

instance Functor f => Functor1 ((:.:) f) where
    fmap1 f = Comp1 . fmap f . unComp1
