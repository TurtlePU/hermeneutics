{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Hermeneutics.SingleSorted where

import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics
import GHC.TypeNats (type (-))
import Hermeneutics.Functors (Functor1 (..))

class Grammar g where
    gmap :: (forall i. a i -> b i) -> g a -> g b
    default gmap ::
        (Generic1 g, Grammar (Rep1 g)) => (forall i. a i -> b i) -> g a -> g b
    gmap f = to1 . gmap f . from1

instance Grammar (K1 i c) where
    gmap _ = K1 . unK1

instance Grammar g => Grammar (M1 i c g) where
    gmap f = M1 . gmap f . unM1

instance (Grammar g, Grammar h) => Grammar (g :+: h) where
    gmap f (L1 g) = L1 (gmap f g)
    gmap f (R1 h) = R1 (gmap f h)

instance (Grammar g, Grammar h) => Grammar (g :*: h) where
    gmap f (g :*: h) = gmap f g :*: gmap f h

instance (Functor f, Grammar g) => Grammar (f :.: g) where
    gmap f = Comp1 . fmap (gmap f) . unComp1

instance Grammar g => Grammar (Rec1 g)

--------------------------------------------------------------------------------

class GFoldable g where
    gfoldMap :: Monoid m => (forall i. a i -> m) -> g a -> m
    default gfoldMap ::
        (Generic1 g, GFoldable (Rep1 g), Monoid m) =>
        (forall i. a i -> m) -> g a -> m
    gfoldMap f = gfoldMap f . from1

instance GFoldable (K1 i c) where
    gfoldMap _ _ = mempty

instance GFoldable g => GFoldable (M1 i c g) where
    gfoldMap f = gfoldMap f . unM1

instance (GFoldable g, GFoldable h) => GFoldable (g :+: h) where
    gfoldMap f (L1 g) = gfoldMap f g
    gfoldMap f (R1 h) = gfoldMap f h

instance (GFoldable g, GFoldable h) => GFoldable (g :*: h) where
    gfoldMap f (g :*: h) = gfoldMap f g <> gfoldMap f h

instance (Foldable f, GFoldable g) => GFoldable (f :.: g) where
    gfoldMap f = foldMap (gfoldMap f) . unComp1

instance GFoldable g => GFoldable (Rec1 g)

--------------------------------------------------------------------------------

class (Grammar g, GFoldable g) => GTraversable g where
    gtraverse :: Applicative f => (forall i. a i -> f (b i)) -> g a -> f (g b)
    default gtraverse ::
        (Generic1 g, GTraversable (Rep1 g), Applicative f) =>
        (forall i. a i -> f (b i)) -> g a -> f (g b)
    gtraverse f = fmap to1 . gtraverse f . from1

instance GTraversable (K1 i c) where
    gtraverse _ = pure . K1 . unK1

instance GTraversable g => GTraversable (M1 i c g) where
    gtraverse f = fmap M1 . gtraverse f . unM1

instance (GTraversable g, GTraversable h) => GTraversable (g :+: h) where
    gtraverse f (L1 g) = L1 <$> gtraverse f g
    gtraverse f (R1 h) = R1 <$> gtraverse f h

instance (GTraversable g, GTraversable h) => GTraversable (g :*: h) where
    gtraverse f (g :*: h) = liftA2 (:*:) (gtraverse f g) (gtraverse f h)

instance (Traversable f, GTraversable g) => GTraversable (f :.: g) where
    gtraverse f = fmap Comp1 . traverse (gtraverse f) . unComp1

instance GTraversable g => GTraversable (Rec1 g)

--------------------------------------------------------------------------------

newtype (:&:) x f = Apply { runApply :: f x }

instance Grammar ((:&:) x) where gmap f = Apply . f . runApply
instance GFoldable ((:&:) x) where gfoldMap f = f . runApply
instance GTraversable ((:&:) x) where gtraverse f = fmap Apply . f . runApply

--------------------------------------------------------------------------------

data Singleton = Sort

type family RepeatSort n where
    RepeatSort 0 = '[]
    RepeatSort n = Sort : RepeatSort (n - 1)

newtype NApp v n = MkNApp { runNApp :: v (Sort :| RepeatSort n) }

newtype SingleSorted g v s = SSorted { runSSorted :: g (NApp v) }

instance Grammar g => Functor1 (SingleSorted g) where
    fmap1 f = SSorted . gmap (MkNApp . f . runNApp) . runSSorted
