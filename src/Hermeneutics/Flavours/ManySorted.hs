module Hermeneutics.Flavours.ManySorted where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Functors (HFunctor (..))

newtype SApp v s = MkSApp { runSApp :: v (s :| '[]) }

newtype ManySorted g v s = MSorted { runMSorted :: g (SApp v) s }

instance HFunctor g => HFunctor (ManySorted g) where
    hmap f = MSorted . hmap (MkSApp . f . runSApp) . runMSorted
