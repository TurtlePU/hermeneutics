module Hermeneutics.Flavours.SecondOrder where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Bifunctor (Bifunctor, bimap)
import Hermeneutics.Functors (HFunctor (..))

data Singleton = Sort

newtype SecondOrder b v s =
    SndOrder { sndOrder :: b (v (Sort :| '[Sort])) (v (Sort :| '[])) }

instance Bifunctor b => HFunctor (SecondOrder b) where
    hmap f = SndOrder . bimap f f . sndOrder
