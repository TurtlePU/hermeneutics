-- |
-- Module      : Hermeneutics.Flavours.ManySorted
-- Description : Adapter for working with many-sorted algebraic grammars.
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains an adapter to lift a non-scoped many-sorted grammar
-- (also called "algebraic" here) into scoped many-sorted grammar.
module Hermeneutics.Flavours.ManySorted where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Flavours (HFoldable (..), HFunctor (..), HTraversable (..))

-- | @'SApp' v@ lowers @v@ from general term provider
-- to the one used in algebraic grammars.
newtype SApp v s = MkSApp { runSApp :: v (s :| '[]) }

-- | Given an algebraic grammar @g@, @'ManySorted' g@ is a scoped grammar
-- defining the same language.
newtype ManySorted g v s = MSorted { runMSorted :: g (SApp v) s }

instance HFunctor g => HFunctor (ManySorted g) where
    hmap f = MSorted . hmap (MkSApp . f . runSApp) . runMSorted

instance HFoldable g => HFoldable (ManySorted g) where
    hfoldMap f = hfoldMap (f . runSApp) . runMSorted

instance HTraversable g => HTraversable (ManySorted g) where
    htraverse f =
        fmap MSorted . htraverse (fmap MkSApp . f . runSApp) . runMSorted
