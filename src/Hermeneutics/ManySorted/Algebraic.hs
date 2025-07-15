module Hermeneutics.ManySorted.Algebraic where

import Hermeneutics.Bound1 (Bound1 (..), bind1)
import Hermeneutics.ManySorted.Tree (Tree)

newtype Id2 f a b = MkId2 { runId2 :: f a b }

instance Bound1 Id2 where
    lift1 f = MkId2 . bind1 f . runId2

-- | Multisorted grammars without bindings
type FreeS = Tree Id2
