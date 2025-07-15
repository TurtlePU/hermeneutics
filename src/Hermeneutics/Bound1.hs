module Hermeneutics.Bound1 where

import Hermeneutics.Functor1 (Functor1, type (~>))

class Functor1 m => Monad1 m where
    unit1 :: a ~> m a
    bind1 :: (a ~> m b) -> (m a ~> m b)

(=>>) :: Monad1 m => m a c -> (a ~> m b) -> m b c
m =>> f = bind1 f m

class Bound1 t where
    lift1 :: Monad1 m => (a ~> m b) -> (t m a ~> t m b)
