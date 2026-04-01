module Hermeneutics.Parser where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

newtype (s :: Type) :~ (r :: Symbol) = Lexeme { sourceText :: s }
