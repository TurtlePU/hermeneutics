{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hermeneutics.Encoding.WellScoped where

import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics ((:+:) (..))
import Hermeneutics.Flavours

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

instance HFunctor g => HFunctor (Scoped g) where
    hmap f (Leaf x) = Leaf (f x)
    hmap f (Node g) = Node (hmap (\(MkExt c s) ->
        MkExt c (hmap (\case { L1 l -> L1 l; R1 r -> R1 (f r)}) s)) g)

instance HFunctor g => HMonad (Scoped g) where
    hpure = Leaf
    hbind f (Leaf x) = f x
    hbind f (Node g) = Node (hmap (\(MkExt c s) -> MkExt c (hbind (\case
            L1 i -> hpure (L1 i)
            R1 v -> hmap R1 (f v)
        ) s)) g)

instance HFoldable g => HFoldable (Scoped g) where
    hfoldMap f (Leaf x) = f x
    hfoldMap f (Node g) = hfoldMap (\(MkExt _ s) -> hfoldMap (\case
            L1 _ -> mempty
            R1 v -> f v
        ) s) g

instance HTraversable g => HTraversable (Scoped g) where
    htraverse f (Leaf x) = Leaf <$> f x
    htraverse f (Node g) = Node <$> htraverse (\(MkExt c s) ->
        MkExt c <$> htraverse (\case
            L1 i -> pure (L1 i)
            R1 v -> R1 <$> f v
        ) s) g

data NAry w ts where
    ZAry :: w s -> NAry w (s :| '[])
    SAry :: (w s -> NAry w (t :| ts)) -> NAry w (t :| s : ts)

evalScoped ::
    forall g v w. HFunctor g => (v ~> w) -> (g (NAry w) ~> w) -> Scoped g v ~> w
evalScoped f _ (Leaf v) = f v
evalScoped f i (Node g) = i (hmap (\(MkExt c e) -> run f c e) g)
  where
    run ::
        (u ~> w) -> Counter ts ->
        Scoped g (In ts :+: u) s -> NAry w (s :| ts)
    run v CZ t = ZAry (evalScoped (in0 v) i t)
    run v (CS c) t = SAry \x ->
        run (\case { This -> x; That y -> v y }) c (hmap in1 t)
