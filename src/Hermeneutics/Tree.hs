{-# LANGUAGE UndecidableInstances #-}

module Hermeneutics.Tree where

import Control.Monad (ap, liftM, (<=<))
import GHC.Generics ((:.:) (..))
import GHC.TypeNats (type (+))
import Hermeneutics.Grammar (Grammar, gmap)
import Control.Category ((<<<))
import Data.Bifunctor (Bifunctor, bimap)
import Data.Bifoldable (Bifoldable, bifoldMap)
import Hermeneutics.GFoldable (GFoldable, gfoldMap)

--------------------------------------------------------------------------------

class Tensor t where
    (>>>=) :: Monad m => t m a -> (a -> m b) -> t m b

--------------------------------------------------------------------------------

data Tree t a = Leaf a | Node (t (Tree t) a)

instance Tensor t => Functor (Tree t) where
    fmap = liftM

instance Tensor t => Applicative (Tree t) where
    pure = Leaf
    (<*>) = ap

instance Tensor t => Monad (Tree t) where
    Leaf a >>= f = f a
    Node n >>= f = Node (n >>>= f)

instance Foldable (t (Tree t)) => Foldable (Tree t) where
    foldMap f (Leaf a) = f a
    foldMap f (Node n) = foldMap f n

--------------------------------------------------------------------------------

instance Functor f => Tensor ((:.:) f) where
    Comp1 a >>>= f = Comp1 $ fmap (>>= f) a

type Free f = Tree ((:.:) f)

--------------------------------------------------------------------------------

newtype BiScoped b m a = MkBiScoped { runBiScoped :: b (m (Maybe a)) (m a) }

instance Bifunctor b => Tensor (BiScoped b) where
    MkBiScoped b >>>= f = MkBiScoped $ bimap (>>= traverse f) (>>= f) b

instance (Bifoldable b, Foldable f) => Foldable (BiScoped b f) where
    foldMap f = bifoldMap (foldMap (foldMap f)) (foldMap f) . runBiScoped

type Foil b = Tree (BiScoped b)

--------------------------------------------------------------------------------

data Fin n where
    FZ :: Fin (n + 1)
    FS :: Fin n -> Fin (n + 1)

newtype Scope a n = MkScope { runScope :: Either (Fin n) a }

newtype Scoped g m a = MkScoped { runScoped :: g (m :.: Scope a) }

instance Grammar g => Tensor (Scoped g) where
    MkScoped g >>>= f = MkScoped $
        gmap (Comp1 <<< fmap MkScope . traverse f . runScope <=< unComp1) g

instance (GFoldable g, Foldable f) => Foldable (Scoped g f) where
    foldMap f = gfoldMap (foldMap (foldMap f . runScope) . unComp1) . runScoped

type Term g = Tree (Scoped g)
