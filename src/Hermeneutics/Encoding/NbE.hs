-- |
-- Module      : Hermeneutics.Encoding.NbE
-- Description : A language of values in a given grammar
--               using de Bruijn levels as variables.
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains a definition of a type of scoped values generated from
-- the given many-sorted grammar with bindings (consult "Hermeneutics.Grammar"
-- for more info on what this means). For variables, de Bruijn levels are used.
module Hermeneutics.Encoding.NbE where

import Data.Functor.Const (Const)
import Hermeneutics.Encoding.DeBruijn (DeBruijn)
import Hermeneutics.Encoding.WellScoped (NAry)
import Hermeneutics.Grammar (type (~>))

-- | Term of scoped values with de Bruijn levels as variables
-- from normalization-by-evaluation approach.
data NbE g l s = NVar (l s) | NNode (g (NAry (NbE g l)) s)

-- | Evaluation part of normalization-by-evaluation.
eval :: DeBruijn g (Const Int) ~> NbE g (Const Int)
eval = error "TODO: NbE eval"

-- | Quoting part of normalization-by-evaluation.
quote :: NbE g (Const Int) ~> DeBruijn g (Const Int)
quote = error "TODO: NbE quote"
