{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hermeneutics.WellScoped where

import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics ((:+:) (..))
import Hermeneutics.Functors (Functor1 (..), Monad1 (..), type (~>))

data In ts t where
    Here :: In (t : ts) t
    There :: In ts t -> In (u : ts) t

in0 :: (v ~> w) -> (In '[] :+: v) ~> w
in0 _ (L1 x) = case x of
in0 f (R1 x) = f x

data Or t v s where
    This :: Or t v t
    That :: v s -> Or t v s

in1 :: (In (t : ts) :+: v) ~> (In ts :+: Or t v)
in1 (L1 Here)      = R1 This
in1 (L1 (There i)) = L1 i
in1 (R1 v)         = R1 (That v)

data Counter ts where
    CZ :: Counter '[]
    CS :: Counter ts -> Counter (s : ts)

data Ext m v ts where
    MkExt :: Counter ts -> m (In ts :+: v) s -> Ext m v (s :| ts)

-- | Multisorted grammars with bindings
data Scoped g v s = Leaf (v s) | Node (g (Ext (Scoped g) v) s)

instance Functor1 g => Functor1 (Scoped g) where
    fmap1 f (Leaf x) = Leaf (f x)
    fmap1 f (Node g) = Node (fmap1 (\(MkExt c s) ->
        MkExt c (fmap1 (\case { L1 l -> L1 l; R1 r -> R1 (f r)}) s)) g)

instance Functor1 g => Monad1 (Scoped g) where
    unit1 = Leaf
    bind1 f (Leaf x) = f x
    bind1 f (Node g) = Node (fmap1 (\(MkExt c s) ->
        MkExt c (bind1 (\case
            L1 i -> unit1 (L1 i)
            R1 v -> fmap1 R1 (f v)
        ) s)) g)

data NAry w ts where
    ZAry :: w s -> NAry w (s :| '[])
    SAry :: (w s -> NAry w (t :| ts)) -> NAry w (t :| s : ts)

evalScoped ::
    forall g v w. Functor1 g => (v ~> w) -> (g (NAry w) ~> w) -> Scoped g v ~> w
evalScoped f _ (Leaf v) = f v
evalScoped f i (Node g) = i (fmap1 (\(MkExt c e) -> run f c e) g)
  where
    run ::
        (u ~> w) -> Counter ts ->
        Scoped g (In ts :+: u) s -> NAry w (s :| ts)
    run v CZ t = ZAry (evalScoped (in0 v) i t)
    run v (CS c) t = SAry \x ->
        run (\case { This -> x; That y -> v y }) c (fmap1 in1 t)
