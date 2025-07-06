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
import Data.Bitraversable (Bitraversable, bitraverse)
import Hermeneutics.GTraversable (GTraversable, gtraverse)

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

instance (Tensor t, Traversable (t (Tree t))) => Traversable (Tree t) where
    traverse f (Leaf a) = fmap Leaf (f a)
    traverse f (Node n) = fmap Node (traverse f n)

--------------------------------------------------------------------------------

instance Functor f => Tensor ((:.:) f) where
    Comp1 a >>>= f = Comp1 $ fmap (>>= f) a

type Free f = Tree ((:.:) f)

--------------------------------------------------------------------------------

newtype BiScoped b m a = MkBiScoped { runBiScoped :: b ((:.:) m Maybe a) (m a) }

instance (Bifunctor b, Functor f) => Functor (BiScoped b f) where
    fmap f = MkBiScoped . bimap (fmap f) (fmap f) . runBiScoped

instance Bifunctor b => Tensor (BiScoped b) where
    MkBiScoped b >>>= f =
        MkBiScoped $ bimap (Comp1 <<< traverse f <=< unComp1) (>>= f) b

instance (Bifoldable b, Foldable f) => Foldable (BiScoped b f) where
    foldMap f = bifoldMap (foldMap f) (foldMap f) . runBiScoped

instance (Bitraversable b, Traversable f) => Traversable (BiScoped b f) where
    traverse f =
        fmap MkBiScoped . bitraverse (traverse f) (traverse f) . runBiScoped

type Foil b = Tree (BiScoped b)

--------------------------------------------------------------------------------

data Fin n where
    FZ :: Fin (n + 1)
    FS :: Fin n -> Fin (n + 1)

newtype Scope m a n = MkScope { runScope :: (:.:) m (Either (Fin n)) a }

unScope :: Scope m a n -> m (Either (Fin n) a)
unScope = unComp1 . runScope

scope :: m (Either (Fin n) a) -> Scope m a n
scope = MkScope . Comp1

newtype Scoped g m a = MkScoped { runScoped :: g (Scope m a) }

instance (Grammar g, Functor f) => Functor (Scoped g f) where
    fmap f = MkScoped . gmap (MkScope . fmap f . runScope) . runScoped

instance Grammar g => Tensor (Scoped g) where
    MkScoped g >>>= f = MkScoped $ gmap (scope <<< traverse f <=< unScope) g

instance (GFoldable g, Foldable f) => Foldable (Scoped g f) where
    foldMap f = gfoldMap (foldMap f . runScope) . runScoped

instance (GTraversable g, Traversable f) => Traversable (Scoped g f) where
    traverse f =
        fmap MkScoped
        . gtraverse (fmap MkScope . traverse f . runScope)
        . runScoped

type Term g = Tree (Scoped g)
