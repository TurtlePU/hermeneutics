{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- |
-- Module      : Hermeneutics.Encoding.Hybrid
-- Description : A language of "terms" in a given grammar where
--               there might be separate binding mechanisms for each sort.
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains a definition of a type of "terms" generated from the
-- given many-sorted grammar with bindings. In addition, each sort may choose
-- their own binding mechanism which is different from the rest.
module Hermeneutics.Encoding.Hybrid where

import Data.List.NonEmpty (NonEmpty (..))

-- | Reification of applying binary operator @b@
-- on sorts in list @ss@ and on seed @m@.
data FoldR b ss m v s where
    FZ :: m v s -> FoldR b '[] m v s
    FS :: b t (FoldR b ss m) v s -> FoldR b (t : ss) m v s

-- | Extension in hybrid context.
data HybridExt b m v ss where
    HExt :: FoldR b ss m v s -> HybridExt b m v (s :| ss)

-- | Hybrid term where different sorts can be bound differently.
data Hybrid b g v s = HVar (v s) | HNode (g (HybridExt b (Hybrid b g) v) s)

-- | Type of a generic substitution operation. Implementation in progress.
substitute :: b t (Hybrid b g) v s -> Hybrid b g v t -> Hybrid b g v s
substitute = error "TODO: generic substitution"
