module Hermeneutics.Monad1 where

import Hermeneutics.Functor1 (Functor1, type (~>))

class Functor1 m => Monad1 m where
    unit1 :: a ~> m a
    bind1 :: (a ~> m b) -> (m a ~> m b)
