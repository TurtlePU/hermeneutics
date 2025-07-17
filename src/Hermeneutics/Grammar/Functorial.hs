-- |
-- Module      : Hermeneutics.Grammar.Functorial
-- Description : Adapter for working with single-sorted algebraic grammars.
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains an adapter to lift a non-scoped single-sorted grammar
-- (presented as a 'Functor') into scoped many-sorted grammar.
module Hermeneutics.Grammar.Functorial where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Grammar (HFoldable (..), HFunctor (..), HTraversable (..))

-- | Given a functor @f@, @'FirstOrder' f@ is a scoped multisorted grammar
-- generating the same language as that functor.
newtype FirstOrder f v s = FstOrder { fstOrder :: f (v (s :| '[])) }

instance Functor f => HFunctor (FirstOrder f) where
    hmap f = FstOrder . fmap f . fstOrder

instance Foldable f => HFoldable (FirstOrder f) where
    hfoldMap f = foldMap f . fstOrder

instance Traversable f => HTraversable (FirstOrder f) where
    htraverse f = fmap FstOrder . traverse f . fstOrder
