{-# LANGUAGE UndecidableInstances #-}

module Hermeneutics.Tree where

import Control.Monad (ap, liftM)
import Control.Monad.Trans.Cont (Cont, cont, runCont)
import Hermeneutics.Bound (Bound (..))
import Hermeneutics.Functor1 (Functor1 (..), type (~>))

data Tree t a = Leaf a | Node (t (Tree t) a)

evalTree ::
    forall a b t. Functor1 t =>
    (a -> b) -> (t (Cont b) ~> Cont b) -> Tree t a -> b
evalTree var f t = runCont (go t) var
  where
    go :: Tree t c -> Cont b c
    go (Leaf a) = cont ($ a)
    go (Node n) = f (fmap1 go n)

instance Bound t => Functor (Tree t) where
    fmap = liftM

instance Bound t => Applicative (Tree t) where
    pure = Leaf
    (<*>) = ap

instance Bound t => Monad (Tree t) where
    Leaf a >>= f = f a
    Node n >>= f = Node (n >>>= f)

instance Foldable (t (Tree t)) => Foldable (Tree t) where
    foldMap f (Leaf a) = f a
    foldMap f (Node n) = foldMap f n

instance (Bound t, Traversable (t (Tree t))) => Traversable (Tree t) where
    traverse f (Leaf a) = fmap Leaf (f a)
    traverse f (Node n) = fmap Node (traverse f n)
