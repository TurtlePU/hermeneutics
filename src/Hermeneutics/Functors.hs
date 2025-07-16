module Hermeneutics.Functors where

import GHC.Generics ((:.:) (..))

type f ~> g = forall a. f a -> g a

class HFunctor t where
    hmap :: (f ~> g) -> (t f ~> t g)

instance Functor f => HFunctor ((:.:) f) where
    hmap f = Comp1 . fmap f . unComp1

class HFunctor m => HMonad m where
    hpure :: a ~> m a
    hbind :: (a ~> m b) -> (m a ~> m b)
