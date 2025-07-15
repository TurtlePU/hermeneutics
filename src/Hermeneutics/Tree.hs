{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Hermeneutics.Tree where

import Control.Category ((<<<))
import Control.Monad (ap, liftM, (<=<))
import Control.Monad.Trans.Cont (Cont, cont, runCont)
import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.Bifunctor (Bifunctor, bimap)
import Data.Bitraversable (Bitraversable, bitraverse)
import GHC.Generics ((:.:) (..))
import GHC.TypeNats (type (+))
import Hermeneutics.Functor1 (Functor1 (..), type (~>))
import Hermeneutics.GFoldable (GFoldable, gfoldMap)
import Hermeneutics.Grammar (Grammar, gmap)
import Hermeneutics.GTraversable (GTraversable, gtraverse)

--------------------------------------------------------------------------------

class Tensor t where
    (>>>=) :: Monad m => t m a -> (a -> m b) -> t m b

--------------------------------------------------------------------------------

data Tree t a = Leaf a | Node (t (Tree t) a)

evalTree ::
    forall a b t. Functor1 t =>
    (a -> b) -> (t (Cont b) ~> Cont b) -> Tree t a -> b
evalTree var f t = runCont (go t) var
  where
    go :: Tree t c -> Cont b c
    go (Leaf a) = cont ($ a)
    go (Node n) = f (fmap1 go n)

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

evalFree :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
evalFree v f = evalTree v (\t -> cont \w -> f (flip runCont w <$> unComp1 t))

--------------------------------------------------------------------------------

newtype BiScoped b m a = MkBiScoped { runBiScoped :: b ((:.:) m Maybe a) (m a) }

instance (Bifunctor b, Functor f) => Functor (BiScoped b f) where
    fmap f = MkBiScoped . bimap (fmap f) (fmap f) . runBiScoped

instance Bifunctor b => Functor1 (BiScoped b) where
    fmap1 f = MkBiScoped . bimap (Comp1 . f . unComp1) f . runBiScoped

instance Bifunctor b => Tensor (BiScoped b) where
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
    f $ bimap ((. (`maybe` w)) . runCont . unComp1) (`runCont` w) $ runBiScoped t

--------------------------------------------------------------------------------

data Fin n where
    FZ :: Fin (n + 1)
    FS :: Fin n -> Fin (n + 1)

peel0 :: Either (Fin 0) a -> a
peel0 = either (\case) id

peel1 :: Either (Fin (n + 1)) a -> Either (Fin n) (Maybe a)
peel1 = either (\case { FZ -> Right Nothing; FS n -> Left n }) (Right . Just)

data Reify n where
    RZ :: Reify 0
    RS :: Reify n -> Reify (n + 1)

data Scope m a n = MkScope
    { scope :: (:.:) m (Either (Fin n)) a
    , reify :: Reify n
    }

type ComposedScope m a n = (:.:) ((,) (Reify n)) ((:.:) m (Either (Fin n))) a

asComposed :: Scope m a n -> ComposedScope m a n
asComposed MkScope {..} = Comp1 (reify, scope)

fromComposed :: ComposedScope m a n -> Scope m a n
fromComposed (Comp1 (reify, scope)) = MkScope {..}

type ScopePair m a n = (Reify n, m (Either (Fin n) a))

asPair :: Scope m a n -> ScopePair m a n
asPair MkScope {..} = (reify, unComp1 scope)

fromPair :: ScopePair m a n -> Scope m a n
fromPair (reify, Comp1 -> scope) = MkScope {..}

newtype Scoped g m a = MkScoped { runScoped :: g (Scope m a) }

instance (Grammar g, Functor f) => Functor (Scoped g f) where
    fmap f = MkScoped . gmap (fromComposed . fmap f . asComposed) . runScoped

instance Grammar g => Functor1 (Scoped g) where
    fmap1 f = MkScoped . gmap (fromPair . fmap f . asPair) . runScoped

instance Grammar g => Tensor (Scoped g) where
    MkScoped g >>>= f =
        MkScoped $ gmap (fromPair . fmap (>>= traverse f) . asPair) g

instance (GFoldable g, Foldable f) => Foldable (Scoped g f) where
    foldMap f = gfoldMap (foldMap f . scope) . runScoped

instance (GTraversable g, Traversable f) => Traversable (Scoped g f) where
    traverse f =
        fmap MkScoped
        . gtraverse (fmap fromComposed . traverse f . asComposed)
        . runScoped

type Term g = Tree (Scoped g)

data NAry a n where
    ZAry :: a -> NAry a 0
    SAry :: (a -> NAry a n) -> NAry a (n + 1)

evalTerm ::
    forall a b g. Grammar g => (a -> b) -> (g (NAry b) -> b) -> Term g a -> b
evalTerm v f = evalTree v \(MkScoped g) ->
    let run :: (c -> b) -> Reify n -> Cont b (Either (Fin n) c) -> NAry b n
        run u RZ t     = ZAry $ runCont t (u . peel0)
        run u (RS r) t = SAry \x -> run (maybe x u) r (peel1 <$> t)
     in cont \w -> f $ gmap (uncurry (run w) . asPair) g
