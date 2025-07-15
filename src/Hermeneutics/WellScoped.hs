{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Hermeneutics.WellScoped where

import Control.Monad.Trans.Cont (Cont, cont, runCont)
import Data.Void (absurd)
import GHC.Generics ((:.:) (..))
import GHC.TypeNats (type (+))
import Hermeneutics.Algebraic (Closed, Fix (..))
import Hermeneutics.Bound (Bound (..))
import Hermeneutics.Functor1 (Functor1 (..))
import Hermeneutics.GFoldable (GFoldable, gfoldMap)
import Hermeneutics.Grammar (Grammar, gmap)
import Hermeneutics.GTraversable (GTraversable, gtraverse)
import Hermeneutics.Tree (Tree, evalTree)

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

instance Grammar g => Bound (Scoped g) where
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

newtype NVF g a = MkNVF { runNVF :: g (NAry a) }

type Value g = Fix (NVF g)

toValue :: Grammar g => Closed (Term g) -> Value g
toValue = evalTerm absurd (MkFix . MkNVF)
