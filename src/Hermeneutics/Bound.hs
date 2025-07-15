module Hermeneutics.Bound where

import GHC.Generics ((:.:) (Comp1))

class Bound t where
    (>>>=) :: Monad m => t m a -> (a -> m b) -> t m b

instance Functor f => Bound ((:.:) f) where
    Comp1 a >>>= f = Comp1 $ fmap (>>= f) a
