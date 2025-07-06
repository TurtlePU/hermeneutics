module Hermeneutics.Tree where

import Control.Monad (ap, liftM, (<=<))
import GHC.Generics ((:.:) (..))
import GHC.TypeNats (type (+))
import Hermeneutics.Grammar (Grammar, gmap)
import Control.Category ((<<<))
import Data.Bifunctor (Bifunctor, bimap)

--------------------------------------------------------------------------------

class Tensor t where
    (>>>=) :: Monad m => t m a -> (a -> m b) -> t m b

--------------------------------------------------------------------------------

data Tree t a = Leaf a | Node (t (Tree t) a)

instance Tensor t => Functor (Tree t) where
    fmap = liftM

instance Tensor t => Applicative (Tree t) where
    pure = Leaf
    (<*>) = ap

instance Tensor t => Monad (Tree t) where
    Leaf a >>= f = f a
    Node n >>= f = Node (n >>>= f)

--------------------------------------------------------------------------------

newtype Alg f m a = MkAlg { runAlg :: f (m a) }

instance Functor f => Tensor (Alg f) where
    MkAlg a >>>= f = MkAlg $ fmap (>>= f) a

type Free f = Tree (Alg f)

--------------------------------------------------------------------------------

newtype BiScoped b m a = MkBiScoped { runBiScoped :: b (m (Maybe a)) (m a) }

instance Bifunctor b => Tensor (BiScoped b) where
    MkBiScoped b >>>= f = MkBiScoped $ bimap (>>= traverse f) (>>= f) b

type Foil b = Tree (BiScoped b)

--------------------------------------------------------------------------------

data Fin n where
    FZ :: Fin (n + 1)
    FS :: Fin n -> Fin (n + 1)

newtype Scope a n = MkScope { runScope :: Either (Fin n) a }

newtype Scoped g m a = MkScoped { runScoped :: g (m :.: Scope a) }

instance Grammar g => Tensor (Scoped g) where
    MkScoped g >>>= f = MkScoped $
        gmap (Comp1 <<< fmap MkScope . traverse f . runScope <=< unComp1) g

type Term g = Tree (Scoped g)
