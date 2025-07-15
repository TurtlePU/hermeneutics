module Hermeneutics.Value where

import Data.Void (Void, absurd)
import Hermeneutics.Tree
import Data.Bifunctor (Bifunctor)
import Hermeneutics.Grammar (Grammar)

type Closed f = f Void

newtype Fix f = MkFix { runFix :: f (Fix f) }

toFix :: Functor f => Closed (Free f) -> Fix f
toFix = evalFree absurd MkFix

newtype BVF b a = MkBVF { runBVF :: b (a -> a) a }

type BValue b = Fix (BVF b)

toBValue :: Bifunctor b => Closed (ScopedFree b) -> BValue b
toBValue = evalScopedFree absurd (MkFix . MkBVF)

newtype NVF g a = MkNVF { runNVF :: g (NAry a) }

type Value g = Fix (NVF g)

toValue :: Grammar g => Closed (Term g) -> Value g
toValue = evalTerm absurd (MkFix . MkNVF)
