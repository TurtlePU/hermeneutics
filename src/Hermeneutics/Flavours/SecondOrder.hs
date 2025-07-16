-- |
-- Module      : Hermeneutics.Flavours.SecondOrder
-- Description : Adapter for working with scoped single-sorted grammars
--               defined via a 'Bifunctor'.
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains an adapter 'SecondOrder' which turns
-- a scoped single-sorted grammar defined via a 'Bifunctor'
-- into a scoped multi-sorted 'HFunctor' that generates the same language.
module Hermeneutics.Flavours.SecondOrder where

import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.Bifunctor (Bifunctor, bimap)
import Data.Bitraversable (Bitraversable, bitraverse)
import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Flavours (HFoldable (..), HFunctor (..), HTraversable (..))

-- | Given a 'Bifunctor' @b@, a @'SecondOrder' b@ is an 'HFunctor' generating
-- the same language.
--
-- In order to derive instances of 'Bifunctor'-like classes, you can use
-- Template Haskell functions provided in the
-- [bifunctors](https://hackage.haskell.org/package/bifunctors) package.
newtype SecondOrder b v s =
    SndOrder { sndOrder :: b (v (s :| '[s])) (v (s :| '[])) }

instance Bifunctor b => HFunctor (SecondOrder b) where
    hmap f = SndOrder . bimap f f . sndOrder

instance Bifoldable b => HFoldable (SecondOrder b) where
    hfoldMap f = bifoldMap f f . sndOrder

instance Bitraversable b => HTraversable (SecondOrder b) where
    htraverse f = fmap SndOrder . bitraverse f f . sndOrder
