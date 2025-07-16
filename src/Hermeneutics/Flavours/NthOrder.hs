{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Hermeneutics.Flavours.NthOrder
-- Description : Adapter for working with scoped single-sorted grammars.
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains definition of 'DFunctor' class useful for defining
-- single-sorted grammars with bindings and an adapter to turn a 'DFunctor'
-- into an 'HFunctor' defining the same grammar.
module Hermeneutics.Flavours.NthOrder where

import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics
import GHC.TypeNats (type (-))
import Hermeneutics.Flavours

-- | 'DFunctor' is a "functor"
-- from "category" of functors of kind @k -> Type@ into Hask.
--
-- While being more general (see its usage in
-- [yaya](https://hackage.haskell.org/package/yaya)), we are particularly
-- interested in the case when @k ~ Natural@ since such functors define a
-- single-sorted grammar with arbitrary (finite) order of bindings:
-- if a 'DFunctor' @g@ is given a functor @v@, usages like @v n@ denote
-- a subterm with @n@ new bindings.
--
-- Deriving via 'Generic1' is available. See ':&:' for details.
--
-- Laws are usual functor laws:
--
-- [Identity] @'dmap' 'id' == 'id'@
-- [Composition] @'dmap' (f '.' g) == 'dmap' f '.' 'dmap' g@
class DFunctor g where
    dmap :: (a ~> b) -> g a -> g b
    default dmap :: (Generic1 g, DFunctor (Rep1 g)) => (a ~> b) -> g a -> g b
    dmap f = to1 . dmap f . from1

instance DFunctor (K1 i c) where
    dmap _ = K1 . unK1

instance DFunctor g => DFunctor (M1 i c g) where
    dmap f = M1 . dmap f . unM1

instance (DFunctor g, DFunctor h) => DFunctor (g :+: h) where
    dmap f (L1 g) = L1 (dmap f g)
    dmap f (R1 h) = R1 (dmap f h)

instance (DFunctor g, DFunctor h) => DFunctor (g :*: h) where
    dmap f (g :*: h) = dmap f g :*: dmap f h

instance (Functor f, DFunctor g) => DFunctor (f :.: g) where
    dmap f = Comp1 . fmap (dmap f) . unComp1

instance DFunctor g => DFunctor (Rec1 g)

--------------------------------------------------------------------------------

-- | Generalization of 'Foldable' to 'DFunctor's.
--
-- Deriving via 'Generic1' is available, see ':&:' for details.
class DFoldable g where
    dfoldMap :: Monoid m => (a /> m) -> g a -> m
    default dfoldMap ::
        (Generic1 g, DFoldable (Rep1 g), Monoid m) => (a /> m) -> g a -> m
    dfoldMap f = dfoldMap f . from1

instance DFoldable (K1 i c) where
    dfoldMap _ _ = mempty

instance DFoldable g => DFoldable (M1 i c g) where
    dfoldMap f = dfoldMap f . unM1

instance (DFoldable g, DFoldable h) => DFoldable (g :+: h) where
    dfoldMap f (L1 g) = dfoldMap f g
    dfoldMap f (R1 h) = dfoldMap f h

instance (DFoldable g, DFoldable h) => DFoldable (g :*: h) where
    dfoldMap f (g :*: h) = dfoldMap f g <> dfoldMap f h

instance (Foldable f, DFoldable g) => DFoldable (f :.: g) where
    dfoldMap f = foldMap (dfoldMap f) . unComp1

instance DFoldable g => DFoldable (Rec1 g)

--------------------------------------------------------------------------------

-- | Generalization of 'Traversable' to 'DFunctor's.
--
-- Deriving via 'Generic1' is available, see ':&:' for details.
class (DFunctor g, DFoldable g) => DTraversable g where
    dtraverse :: Applicative f => Klei f a b -> g a -> f (g b)
    default dtraverse ::
        (Generic1 g, DTraversable (Rep1 g), Applicative f) =>
        Klei f a b -> g a -> f (g b)
    dtraverse f = fmap to1 . dtraverse f . from1

instance DTraversable (K1 i c) where
    dtraverse _ = pure . K1 . unK1

instance DTraversable g => DTraversable (M1 i c g) where
    dtraverse f = fmap M1 . dtraverse f . unM1

instance (DTraversable g, DTraversable h) => DTraversable (g :+: h) where
    dtraverse f (L1 g) = L1 <$> dtraverse f g
    dtraverse f (R1 h) = R1 <$> dtraverse f h

instance (DTraversable g, DTraversable h) => DTraversable (g :*: h) where
    dtraverse f (g :*: h) = liftA2 (:*:) (dtraverse f g) (dtraverse f h)

instance (Traversable f, DTraversable g) => DTraversable (f :.: g) where
    dtraverse f = fmap Comp1 . traverse (dtraverse f) . unComp1

instance DTraversable g => DTraversable (Rec1 g)

--------------------------------------------------------------------------------

-- | A helpful newtype for 'DFunctor' family of classes via 'Generic1'.
--
-- Due to the limitations of 'Generic1' deriving, GHC cannot derive 'Generic1'
-- for grammars like this:
--
-- >>> data ManualLC v = MApp (v 0) (v 0) | MAbs (v 1)
--
-- However, using the ':&:' newtype, derivation would work just fine:
--
-- >>> data DerivableLC v = DApp (0 :&: v) (0 :&: v) | DAbs (1 :&: v)
-- >>>     deriving Generic1
-- >>> instance DFunctor DerivableLC
-- >>> instance DFoldable DerivableLC
-- >>> instance DTraversable DerivableLC
newtype (:&:) x f = Apply { runApply :: f x }

instance DFunctor ((:&:) x) where dmap f = Apply . f . runApply
instance DFoldable ((:&:) x) where dfoldMap f = f . runApply
instance DTraversable ((:&:) x) where dtraverse f = fmap Apply . f . runApply

--------------------------------------------------------------------------------

-- | @'Repeat' s n@ creates a type-level list
-- consisting of @s@ of length exactly @n@.
type family Repeat s n where
    Repeat _ 0 = '[]
    Repeat s n = s : Repeat s (n - 1)

-- | @'NApp' v s@ lowers @v@ from general term provider to the one
-- used in 'DFunctor's, using @s@ as a single sort used throughout.
newtype NApp v s n = MkNApp { runNApp :: v (s :| Repeat s n) }

-- | Given a 'DFunctor' @g@, @'NthOrder' g@ is an 'HFunctor'
-- defining the same grammar.
newtype NthOrder g v s = MkNthOrder { runNthOrder :: g (NApp v s) }

instance DFunctor g => HFunctor (NthOrder g) where
    hmap f = MkNthOrder . dmap (MkNApp . f . runNApp) . runNthOrder

instance DFoldable g => HFoldable (NthOrder g) where
    hfoldMap f = dfoldMap (f . runNApp) . runNthOrder

instance DTraversable g => HTraversable (NthOrder g) where
    htraverse f =
        fmap MkNthOrder . dtraverse (fmap MkNApp . f . runNApp) . runNthOrder
