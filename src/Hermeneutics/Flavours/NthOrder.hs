{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Hermeneutics.Flavours.NthOrder where

import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics
import GHC.TypeNats (type (-))
import Hermeneutics.Functors (Functor1 (..), type (~>))

class NGrammar g where
    nmap :: (a ~> b) -> g a -> g b
    default nmap ::
        (Generic1 g, NGrammar (Rep1 g)) => (forall i. a i -> b i) -> g a -> g b
    nmap f = to1 . nmap f . from1

instance NGrammar (K1 i c) where
    nmap _ = K1 . unK1

instance NGrammar g => NGrammar (M1 i c g) where
    nmap f = M1 . nmap f . unM1

instance (NGrammar g, NGrammar h) => NGrammar (g :+: h) where
    nmap f (L1 g) = L1 (nmap f g)
    nmap f (R1 h) = R1 (nmap f h)

instance (NGrammar g, NGrammar h) => NGrammar (g :*: h) where
    nmap f (g :*: h) = nmap f g :*: nmap f h

instance (Functor f, NGrammar g) => NGrammar (f :.: g) where
    nmap f = Comp1 . fmap (nmap f) . unComp1

instance NGrammar g => NGrammar (Rec1 g)

--------------------------------------------------------------------------------

class NFoldable g where
    nfoldMap :: Monoid m => (forall i. a i -> m) -> g a -> m
    default nfoldMap ::
        (Generic1 g, NFoldable (Rep1 g), Monoid m) =>
        (forall i. a i -> m) -> g a -> m
    nfoldMap f = nfoldMap f . from1

instance NFoldable (K1 i c) where
    nfoldMap _ _ = mempty

instance NFoldable g => NFoldable (M1 i c g) where
    nfoldMap f = nfoldMap f . unM1

instance (NFoldable g, NFoldable h) => NFoldable (g :+: h) where
    nfoldMap f (L1 g) = nfoldMap f g
    nfoldMap f (R1 h) = nfoldMap f h

instance (NFoldable g, NFoldable h) => NFoldable (g :*: h) where
    nfoldMap f (g :*: h) = nfoldMap f g <> nfoldMap f h

instance (Foldable f, NFoldable g) => NFoldable (f :.: g) where
    nfoldMap f = foldMap (nfoldMap f) . unComp1

instance NFoldable g => NFoldable (Rec1 g)

--------------------------------------------------------------------------------

class (NGrammar g, NFoldable g) => NTraversable g where
    ntraverse :: Applicative f => (forall i. a i -> f (b i)) -> g a -> f (g b)
    default ntraverse ::
        (Generic1 g, NTraversable (Rep1 g), Applicative f) =>
        (forall i. a i -> f (b i)) -> g a -> f (g b)
    ntraverse f = fmap to1 . ntraverse f . from1

instance NTraversable (K1 i c) where
    ntraverse _ = pure . K1 . unK1

instance NTraversable g => NTraversable (M1 i c g) where
    ntraverse f = fmap M1 . ntraverse f . unM1

instance (NTraversable g, NTraversable h) => NTraversable (g :+: h) where
    ntraverse f (L1 g) = L1 <$> ntraverse f g
    ntraverse f (R1 h) = R1 <$> ntraverse f h

instance (NTraversable g, NTraversable h) => NTraversable (g :*: h) where
    ntraverse f (g :*: h) = liftA2 (:*:) (ntraverse f g) (ntraverse f h)

instance (Traversable f, NTraversable g) => NTraversable (f :.: g) where
    ntraverse f = fmap Comp1 . traverse (ntraverse f) . unComp1

instance NTraversable g => NTraversable (Rec1 g)

--------------------------------------------------------------------------------

newtype (:&:) x f = Apply { runApply :: f x }

instance NGrammar ((:&:) x) where nmap f = Apply . f . runApply
instance NFoldable ((:&:) x) where nfoldMap f = f . runApply
instance NTraversable ((:&:) x) where ntraverse f = fmap Apply . f . runApply

--------------------------------------------------------------------------------

data Singleton = Sort

type family RepeatSort n where
    RepeatSort 0 = '[]
    RepeatSort n = Sort : RepeatSort (n - 1)

newtype NApp v n = MkNApp { runNApp :: v (Sort :| RepeatSort n) }

newtype NthOrder g v s = MkNthOrder { runNthOrder :: g (NApp v) }

instance NGrammar g => Functor1 (NthOrder g) where
    fmap1 f = MkNthOrder . nmap (MkNApp . f . runNApp) . runNthOrder
