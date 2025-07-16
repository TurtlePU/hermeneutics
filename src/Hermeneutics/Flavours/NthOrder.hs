{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Hermeneutics.Flavours.NthOrder where

import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics
import GHC.TypeNats (type (-))
import Hermeneutics.Flavours.SecondOrder (Singleton (..))
import Hermeneutics.Functors (HFunctor (..), type (~>))

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

class DFoldable g where
    dfoldMap :: Monoid m => (forall i. a i -> m) -> g a -> m
    default dfoldMap ::
        (Generic1 g, DFoldable (Rep1 g), Monoid m) =>
        (forall i. a i -> m) -> g a -> m
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

class (DFunctor g, DFoldable g) => DTraversable g where
    dtraverse :: Applicative f => (forall i. a i -> f (b i)) -> g a -> f (g b)
    default dtraverse ::
        (Generic1 g, DTraversable (Rep1 g), Applicative f) =>
        (forall i. a i -> f (b i)) -> g a -> f (g b)
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

newtype (:&:) x f = Apply { runApply :: f x }

instance DFunctor ((:&:) x) where dmap f = Apply . f . runApply
instance DFoldable ((:&:) x) where dfoldMap f = f . runApply
instance DTraversable ((:&:) x) where dtraverse f = fmap Apply . f . runApply

--------------------------------------------------------------------------------

type family RepeatSort n where
    RepeatSort 0 = '[]
    RepeatSort n = Sort : RepeatSort (n - 1)

newtype NApp v n = MkNApp { runNApp :: v (Sort :| RepeatSort n) }

newtype NthOrder g v s = MkNthOrder { runNthOrder :: g (NApp v) }

instance DFunctor g => HFunctor (NthOrder g) where
    hmap f = MkNthOrder . dmap (MkNApp . f . runNApp) . runNthOrder
