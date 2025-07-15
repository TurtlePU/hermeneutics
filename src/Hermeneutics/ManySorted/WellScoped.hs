{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hermeneutics.ManySorted.WellScoped where

import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics ((:+:) (..))
import Hermeneutics.Bound1 (Bound1 (..), unit1, (=>>))
import Hermeneutics.Functor1 (fmap1, type (~>))
import Hermeneutics.Grammar (Grammar, gmap)
import Hermeneutics.ManySorted.Tree (Tree (..))

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

instance Bound1 Ext where
    lift1 f (MkExt c m) = MkExt c $ m =>> \case
        L1 i -> unit1 (L1 i)
        R1 v -> f v =>> (unit1 . R1)

-- | Multisorted grammars with bindings
type Scoped = Tree Ext

data NAry w ts where
    ZAry :: w s -> NAry w (s :| '[])
    SAry :: (w s -> NAry w (t :| ts)) -> NAry w (t :| s : ts)

evalScoped ::
    forall g v w. (forall s. Grammar (g s)) =>
    (v ~> w) -> (forall s. g s (NAry w) -> w s) -> Scoped g v ~> w
evalScoped f _ (Leaf v) = f v
evalScoped f i (Node g) = i (gmap (\(MkExt c e) -> run f c e) g)
  where
    run ::
        (u ~> w) -> Counter ts ->
        Tree Ext g (In ts :+: u) s -> NAry w (s :| ts)
    run v CZ t = ZAry (evalScoped (in0 v) i t)
    run v (CS c) t = SAry \x ->
        run (\case { This -> x; That y -> v y }) c (fmap1 in1 t)
