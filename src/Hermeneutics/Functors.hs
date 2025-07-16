module Hermeneutics.Functors where

import GHC.Generics ((:.:) (..))

type a ~> b = forall i. a i -> b i

class HFunctor f where
    hmap :: (a ~> b) -> (f a ~> f b)

instance Functor f => HFunctor ((:.:) f) where
    hmap f = Comp1 . fmap f . unComp1

class HFunctor m => HMonad m where
    hpure :: a ~> m a
    hbind :: (a ~> m b) -> (m a ~> m b)
