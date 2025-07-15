module Hermeneutics.ManySorted.WellScoped where

import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics ((:+:) (..))
import Hermeneutics.Bound1 (Bound1 (..), unit1, (=>>))
import Hermeneutics.ManySorted.Tree (Tree)

data In ts t where
    Here :: In (t : ts) t
    There :: In ts t -> In (u : ts) t

data Ext m v ts where
    MkExt :: m (In ts :+: v) s -> Ext m v (s :| ts)

instance Bound1 Ext where
    lift1 f (MkExt m) = MkExt $ m =>> \case
        L1 i -> unit1 (L1 i)
        R1 v -> f v =>> (unit1 . R1)

-- | Multisorted grammars with bindings
type Scoped = Tree Ext
