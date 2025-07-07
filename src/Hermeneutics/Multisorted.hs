{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BlockArguments #-}

module Hermeneutics.Multisorted where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Grammar (Grammar (..))
import GHC.Generics ((:+:) (..))

--------------------------------------------------------------------------------

data Tree t g v s = Leaf (v s) | Node (g s (t (Tree t g) v))

type f ~> g = forall a. f a -> g a

class Monad1 m where
    unit1 :: a ~> m a
    bind1 :: (a ~> m b) -> (m a ~> m b)

(=>>) :: Monad1 m => m a c -> (a ~> m b) -> m b c
m =>> f = bind1 f m

class Tensor1 t where
    lift1 :: Monad1 m => (a ~> m b) -> (t m a ~> t m b)

instance (Tensor1 t, forall s. Grammar (g s)) => Monad1 (Tree t g) where
    unit1 = Leaf
    bind1 f (Leaf u) = f u
    bind1 f (Node n) = Node (gmap (lift1 f) n)

--------------------------------------------------------------------------------

newtype Id2 f a b = MkId2 { runId2 :: f a b }

instance Tensor1 Id2 where
    lift1 f = MkId2 . bind1 f . runId2

-- | Multisorted grammars without bindings
type FreeS = Tree Id2

--------------------------------------------------------------------------------

data In ts t where
    Here :: In (t : ts) t
    There :: In ts t -> In (u : ts) t

data Ext m v ts where
    MkExt :: m (In ts :+: v) s -> Ext m v (s :| ts)

instance Tensor1 Ext where
    lift1 f (MkExt m) = MkExt $ m =>> \case
        L1 i -> unit1 (L1 i)
        R1 v -> f v =>> (unit1 . R1)

-- | Multisorted grammars with bindings
type Scoped = Tree Ext
