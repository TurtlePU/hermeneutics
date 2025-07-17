-- |
-- Module      : Hermeneutics.Grammar.Categorical
-- Description : Adapter for working with many-sorted single-scoped grammars.
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains an adapter to lift a single-scoped many-sorted grammar
-- (also called /categorical/ here) into scoped many-sorted grammar.
--
-- Please NOTE that /categorical grammars/ defined here have no relation
-- whatsoever to the same-named grammars studied in linguistics.
module Hermeneutics.Grammar.Categorical where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Grammar

-- | A shorthand for natural transformation between bifunctors.
type c ~~> d = forall i j. c i j -> d i j

-- | A shorthand for natural bifunctor consumer.
type c //> d = forall i j. c i j -> d

-- | A shorthand for natural Kleisli transformation between bifunctors.
type Kleii f c d = forall i j. c i j -> f (d i j)

-- | A 'CFunctor' is a Bifunctor where first argument comes from category of
-- bifunctors, whereas second argument comes from category of functors.
--
-- A name comes from the impression that such functors accept
-- /category presentations/: first argument is presentation of hom-sets, and
-- second argument is presentation of objects.
class CFunctor b where
    cmap :: (c ~~> d) -> (o ~> p) -> (b c o ~> b d p)

-- | A generalization of Bifoldable in analogy with 'CFunctor'.
class CFoldable b where
    cfoldMap :: Monoid m => (c //> m) -> (o /> m) -> (b c o /> m)

-- | A generalization of Bitraversable in analogy with 'CFunctor'.
class (CFunctor b, CFoldable b) => CTraversable b where
    ctraverse ::
        Applicative f => Kleii f c d -> Klei f o p -> Klei f (b c o) (b d p)

-- | @'Hom' v@ lowers @v@ from general term provider to the /hom-set/ term
-- provider for categorical grammar.
newtype Hom v s t = MkHom { runHom :: v (s :| '[t]) }

-- | @'Obj' v@ lowers @v@ from general term provider to the /object/ term
-- provider for categorical grammar.
newtype Obj v s = MkObj { runObj :: v (s :| '[]) }

-- | Given a categorical grammar @b@, @'Categorical' b@ is a scoped many-sorted
-- grammar generating the same language.
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
