-- |
-- Module      : Hermeneutics.Parser
-- Description : An implementation of resilient lexer+parser.
-- Copyright   : (c) TurtlePU, 2026
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module re-exports public API for easy generation of lexers and parsers
-- from CST (Concrete Syntax Tree) written as an algebraic datatype.
module Hermeneutics.Parser where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

-- | Type of lexemes. In @s ':~' r@, @r@ is a regular expression written inside
-- Haskell's 'Symbol', and @s@ is a type of the source stream
-- (be it @Text@, @ByteString@ or something else).
newtype (s :: Type) :~ (r :: Symbol) = Lexeme { sourceText :: s }
