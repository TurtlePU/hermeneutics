{-# LANGUAGE BlockArguments #-}

module Hermeneutics.Algebraic where

import Control.Monad.Trans.Cont (cont, runCont)
import Data.Void (Void, absurd)
import GHC.Generics (unComp1, (:.:))
import Hermeneutics.Tree (Tree, evalTree)

type Free f = Tree ((:.:) f)

evalFree :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
evalFree v f = evalTree v (\t -> cont \w -> f (flip runCont w <$> unComp1 t))

newtype Fix f = MkFix { runFix :: f (Fix f) }

type Closed f = f Void

toFix :: Functor f => Closed (Free f) -> Fix f
toFix = evalFree absurd MkFix
