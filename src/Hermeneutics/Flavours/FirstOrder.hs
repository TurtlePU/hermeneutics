-- |
-- Module      : Hermeneutics.Flavours.FirstOrder
-- Description : Adapter for working with single-sorted algebraic grammars.
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains an adapter to lift a non-scoped single-sorted grammar
-- (also called "algebraic" here) into scoped many-sorted grammar.
module Hermeneutics.Flavours.FirstOrder where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Flavours (HFoldable (..), HFunctor (..), HTraversable (..))

-- | Given a functor @f@, @'FirstOrder' f@ is a scoped multisorted grammar
-- generating the same language.
newtype FirstOrder f v s = FstOrder { fstOrder :: f (v (s :| '[])) }

instance Functor f => HFunctor (FirstOrder f) where
    hmap f = FstOrder . fmap f . fstOrder

instance Foldable f => HFoldable (FirstOrder f) where
    hfoldMap f = foldMap f . fstOrder

instance Traversable f => HTraversable (FirstOrder f) where
    htraverse f = fmap FstOrder . traverse f . fstOrder
