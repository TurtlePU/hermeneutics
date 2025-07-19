-- |
-- Module      : Hermeneutics.Encoding.Value
-- Description : A type of values in a grammar.
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains a definition of a type of "values" generated
-- from the given many-sorted grammar with bindings (consult
-- "Hermeneutics.Grammar" for more info on what this means).
module Hermeneutics.Encoding.Value where

import Hermeneutics.Encoding.WellScoped (NAry, Scoped, evalScoped)
import Data.Functor.Const (Const (..))
import Data.Void (Void, absurd)
import Hermeneutics.Grammar (HFunctor)

-- | Type of "values" generated from the given many-sorted grammar with bindings.
--
-- This encoding is "final" in a sense, meaning that no other encoding can be
-- usually obtained from it. Its main usecase is evaluation of terms in a naive,
-- tree-walking way. Bindings are encoded using Haskell functions, so
-- substitution should be pretty efficient.
newtype Value g s = MkValue { runValue :: g (NAry (Value g)) s }

-- | Obtain a 'Value' from closed well-'Scoped' term.
fromClosed :: HFunctor g => Scoped g (Const Void) s -> Value g s
fromClosed = evalScoped (absurd . getConst) MkValue
