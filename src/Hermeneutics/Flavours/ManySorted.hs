module Hermeneutics.Flavours.ManySorted where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Functors (Functor1 (..))

newtype SApp v s = MkSApp { runSApp :: v (s :| '[]) }

newtype ManySorted g v s = MSorted { runMSorted :: g (SApp v) s }

instance Functor1 g => Functor1 (ManySorted g) where
    fmap1 f = MSorted . fmap1 (MkSApp . f . runSApp) . runMSorted
