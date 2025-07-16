module Hermeneutics.Flavours.Categorical where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Functors (type (~>), HFunctor (..))

type c ~~> d = forall i j. c i j -> d i j

class CFunctor b where
    cmap :: (c ~~> d) -> (o ~> p) -> (b c o ~> b d p)

newtype Hom v s t = MkHom { runHom :: v (s :| '[t]) }

newtype Obj v s = MkObj { runObj :: v (s :| '[]) }

newtype Categorical b v s =
    MkCategorical { runCategorical :: b (Hom v) (Obj v) s }

instance CFunctor b => HFunctor (Categorical b) where
    hmap f =
        MkCategorical
        . cmap (MkHom . f . runHom) (MkObj . f . runObj)
        . runCategorical
