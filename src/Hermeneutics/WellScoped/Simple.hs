{-# LANGUAGE BlockArguments #-}

module Hermeneutics.WellScoped.Simple where

import Control.Category ((<<<))
import Control.Monad ((<=<))
import Control.Monad.Trans.Cont (cont, runCont)
import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.Bifunctor (Bifunctor, bimap)
import Data.Bitraversable (Bitraversable, bitraverse)
import Data.Void (absurd)
import GHC.Generics ((:.:) (..))
import Hermeneutics.Algebraic (Closed, Fix (..))
import Hermeneutics.Bound (Bound (..))
import Hermeneutics.Functor1 (Functor1 (..))
import Hermeneutics.Tree (Tree, evalTree)

newtype BiScoped b m a = MkBiScoped { runBiScoped :: b ((:.:) m Maybe a) (m a) }

instance (Bifunctor b, Functor f) => Functor (BiScoped b f) where
    fmap f = MkBiScoped . bimap (fmap f) (fmap f) . runBiScoped

instance Bifunctor b => Functor1 (BiScoped b) where
    fmap1 f = MkBiScoped . bimap (Comp1 . f . unComp1) f . runBiScoped

instance Bifunctor b => Bound (BiScoped b) where
    MkBiScoped b >>>= f =
        MkBiScoped $ bimap (Comp1 <<< traverse f <=< unComp1) (>>= f) b

instance (Bifoldable b, Foldable f) => Foldable (BiScoped b f) where
    foldMap f = bifoldMap (foldMap f) (foldMap f) . runBiScoped

instance (Bitraversable b, Traversable f) => Traversable (BiScoped b f) where
    traverse f =
        fmap MkBiScoped . bitraverse (traverse f) (traverse f) . runBiScoped

type ScopedFree b = Tree (BiScoped b)

evalScopedFree ::
    Bifunctor b => (a -> c) -> (b (c -> c) c -> c) -> ScopedFree b a -> c
evalScopedFree v f = evalTree v \t -> cont \w ->
    f $ bimap ((. (`maybe` w)) . runCont . unComp1) (`runCont` w)
        $ runBiScoped t

newtype BVF b a = MkBVF { runBVF :: b (a -> a) a }

type BValue b = Fix (BVF b)

toBValue :: Bifunctor b => Closed (ScopedFree b) -> BValue b
toBValue = evalScopedFree absurd (MkFix . MkBVF)
