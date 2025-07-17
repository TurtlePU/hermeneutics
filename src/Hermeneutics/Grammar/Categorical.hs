module Hermeneutics.Grammar.Categorical where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Grammar

type c ~~> d = forall i j. c i j -> d i j

type c //> d = forall i j. c i j -> d

type Kleii f c d = forall i j. c i j -> f (d i j)

class CFunctor b where
    cmap :: (c ~~> d) -> (o ~> p) -> (b c o ~> b d p)

class CFoldable b where
    cfoldMap :: Monoid m => (c //> m) -> (o /> m) -> (b c o /> m)

class (CFunctor b, CFoldable b) => CTraversable b where
    ctraverse ::
        Applicative f => Kleii f c d -> Klei f o p -> Klei f (b c o) (b d p)

newtype Hom v s t = MkHom { runHom :: v (s :| '[t]) }

newtype Obj v s = MkObj { runObj :: v (s :| '[]) }

newtype Categorical b v s =
    MkCategorical { runCategorical :: b (Hom v) (Obj v) s }

instance CFunctor b => HFunctor (Categorical b) where
    hmap f =
        MkCategorical
        . cmap (MkHom . f . runHom) (MkObj . f . runObj)
        . runCategorical

instance CFoldable b => HFoldable (Categorical b) where
    hfoldMap f = cfoldMap (f . runHom) (f . runObj) . runCategorical

instance CTraversable b => HTraversable (Categorical b) where
    htraverse f =
        fmap MkCategorical
        . ctraverse (fmap MkHom . f . runHom) (fmap MkObj . f . runObj)
        . runCategorical
