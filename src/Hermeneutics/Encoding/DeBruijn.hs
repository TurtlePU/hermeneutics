-- |
-- Module      : Hermeneutics.Encoding.DeBruijn
-- Description : A language of terms in a given grammar
--               using de Bruijn indices as variables.
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains a definition of a type of scoped terms generated from
-- the given many-sorted grammar with bindings (consult "Hermeneutics.Grammar"
-- for more info on what this means). For variables, de Bruijn indices are used.
module Hermeneutics.Encoding.DeBruijn where

import Data.Functor.Const (Const (Const))
import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Encoding.WellScoped (Counter (..))
import Hermeneutics.Grammar (HFunctor (..))
import Hermeneutics.Grammar.NthOrder (DFunctor, NApp (..), NthOrder (..), dmap)

-- | Extension of context, de Bruijn style.
data DeBruijnExt m i ss where
    DBExt :: Counter ss -> m i s -> DeBruijnExt m i (s ':| ss)

instance HFunctor m => HFunctor (DeBruijnExt m) where
    hmap f (DBExt c m) = c `DBExt` hmap f m

-- | Type of terms using de Bruijn indices for variables.
data DeBruijn g i s = DVar (i s) | DNode (g (DeBruijnExt (DeBruijn g) i) s)

instance HFunctor g => HFunctor (DeBruijn g) where
    hmap f = go
      where
        go (DVar x)  = DVar (f x)
        go (DNode n) = DNode (hmap (hmap f) n)

-- | Single-sorted de Bruijn terms.
type SimpleDBn g = DeBruijn (NthOrder g) (Const Int) '()

-- | Substitution on single-sorted de Bruijn terms.
-- In @subst f t@, @f@ is a scoped term, and @t@ is a closed term that's being
-- substituted into @f@.
subst :: forall g. DFunctor g => SimpleDBn g -> SimpleDBn g -> SimpleDBn g
subst f = shift pred . replace 0 f . shift succ
  where
    shift :: (Int -> Int) -> SimpleDBn g -> SimpleDBn g
    shift g = go 0
      where
        go :: Int -> SimpleDBn g -> SimpleDBn g
        go t (DVar (Const x))
            | x >= t = DVar $ Const (g x)
            | otherwise = DVar (Const x)
        go t (DNode (MkNthOrder n)) = DNode $ MkNthOrder $
            dmap (\(MkNApp (DBExt c s)) ->
                MkNApp $ DBExt c $ go (t + count c) s
                ) n

    replace :: Int -> SimpleDBn g -> SimpleDBn g -> SimpleDBn g
    replace t (DVar (Const x)) what
      | x == t = what
      | otherwise = DVar (Const x)
    replace t (DNode (MkNthOrder n)) what = DNode $ MkNthOrder $
      dmap (\(MkNApp (DBExt c s)) ->
          MkNApp $ DBExt c $ replace (t + count c) s (shift (+ count c) what)
        ) n

    count :: Counter ss -> Int
    count CZ     = 0
    count (CS c) = 1 + count c
