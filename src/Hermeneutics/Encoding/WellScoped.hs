{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- |
-- Module      : Hermeneutics.Encoding.WellScoped
-- Description : A language of well-scoped terms in a given grammar.
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains a definition of a type of well-scoped terms generated
-- from the given many-sorted grammar with bindings (consult
-- "Hermeneutics.Grammar" for more info on what this means).
module Hermeneutics.Encoding.WellScoped where

import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics ((:+:) (..))
import Hermeneutics.Grammar

-- | Each instance of @'In' ts t@
-- is a witness of @t@ as an element in list @ts@.
data In ts t where
    Here :: In (t : ts) t
    There :: In ts t -> In (u : ts) t

-- | There are no elements in an empty list,
-- therefore such evidence can be discarded.
in0 :: (v ~> w) -> (In '[] :+: v) ~> w
in0 _ (L1 x) = case x of
in0 f (R1 x) = f x

-- | If @'Or' t v s@, then either @t ~ s@, or @v s@ holds.
data Or t v s where
    This :: Or t v t
    That :: v s -> Or t v s

-- | If @'In' (t : ts) s@, then either @t ~ s@, or @'In' ts s@.
in1 :: (In (t : ts) :+: v) ~> (In ts :+: Or t v)
in1 (L1 Here)      = R1 This
in1 (L1 (There i)) = L1 i
in1 (R1 v)         = R1 (That v)

-- | A term-level reification of type-level list length.
data Counter ts where
    CZ :: Counter '[]
    CS :: Counter ts -> Counter (s : ts)

-- | @'Ext' m v (s :| ts)@ is a term @m@ of sort @s@
-- in scope @v@ extended with variables of sorts @ts@.
data Ext m v ts where
    MkExt :: Counter ts -> m (In ts :+: v) s -> Ext m v (s :| ts)

-- | @'Scoped' g v s@ is a type of terms of sort @s@ in scope @v@ generated from
-- the grammar @g@.
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

-- | @'NAry' w ts@ is a type of N-ary functions
-- where sorts of arguments and result are recorded in @ts@
-- and presentation of sorts as types is given as @w@.
data NAry w ts where
    ZAry :: w s -> NAry w (s :| '[])
    SAry :: (w s -> NAry w (t :| ts)) -> NAry w (t :| s : ts)

-- | Given an evaluation of variables in scope and a way to evaluate grammar,
-- evaluates the term.
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
