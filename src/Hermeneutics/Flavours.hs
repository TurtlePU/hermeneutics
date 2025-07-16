module Hermeneutics.Flavours where

import GHC.Generics ((:.:) (..))

type a ~> b = forall i. a i -> b i

type a /> b = forall i. a i -> b

type Klei f a b = forall i. a i -> f (b i)

class HFunctor f where
    hmap :: (a ~> b) -> (f a ~> f b)

instance Functor f => HFunctor ((:.:) f) where
    hmap f = Comp1 . fmap f . unComp1

class HFunctor m => HMonad m where
    hpure :: a ~> m a
    hbind :: (a ~> m b) -> (m a ~> m b)

class HFoldable f where
    hfoldMap :: Monoid m => (a /> m) -> f a /> m

class (HFunctor t, HFoldable t) => HTraversable t where
    htraverse :: Applicative f => Klei f a b -> Klei f (t a) (t b)
