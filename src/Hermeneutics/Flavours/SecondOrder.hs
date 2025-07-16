module Hermeneutics.Flavours.SecondOrder where

import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.Bifunctor (Bifunctor, bimap)
import Data.Bitraversable (Bitraversable, bitraverse)
import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Flavours (HFoldable (..), HFunctor (..), HTraversable (..))

newtype SecondOrder b v s =
    SndOrder { sndOrder :: b (v (s :| '[s])) (v (s :| '[])) }

instance Bifunctor b => HFunctor (SecondOrder b) where
    hmap f = SndOrder . bimap f f . sndOrder

instance Bifoldable b => HFoldable (SecondOrder b) where
    hfoldMap f = bifoldMap f f . sndOrder

instance Bitraversable b => HTraversable (SecondOrder b) where
    htraverse f = fmap SndOrder . bitraverse f f . sndOrder
