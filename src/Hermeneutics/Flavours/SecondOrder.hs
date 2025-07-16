module Hermeneutics.Flavours.SecondOrder where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Bifunctor (Bifunctor, bimap)
import Hermeneutics.Functors (HFunctor (..))

newtype SecondOrder b v s =
    SndOrder { sndOrder :: b (v (s :| '[s])) (v (s :| '[])) }

instance Bifunctor b => HFunctor (SecondOrder b) where
    hmap f = SndOrder . bimap f f . sndOrder
