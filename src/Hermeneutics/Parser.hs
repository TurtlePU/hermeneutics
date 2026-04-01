module Hermeneutics.Parser where

import GHC.TypeLits (Symbol)
import Data.Kind (Type)

newtype (s :: Type) :~ (r :: Symbol) = Lexeme { sourceText :: s }
