module Hermeneutics.Encoding.DeBruijn where

import Data.Functor.Const (Const (Const))
import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Encoding.WellScoped (Counter (..))
import Hermeneutics.Grammar (HFunctor (..))
import Hermeneutics.Grammar.NthOrder (NthOrder (MkNthOrder), dmap, NApp (..), DFunctor)

data DeBruijnExt m i ss where
    DBExt :: Counter ss -> m i s -> DeBruijnExt m i (s ':| ss)

instance HFunctor m => HFunctor (DeBruijnExt m) where
    hmap f (DBExt c m) = c `DBExt` hmap f m

data DeBruijn g i s = DVar (i s) | DNode (g (DeBruijnExt (DeBruijn g) i) s)

instance HFunctor g => HFunctor (DeBruijn g) where
    hmap f = go
      where
        go (DVar x)  = DVar (f x)
        go (DNode n) = DNode (hmap (hmap f) n)

type SimpleDBn g = DeBruijn (NthOrder g) (Const Int) '()

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
    count CZ = 0
    count (CS c) = 1 + count c
