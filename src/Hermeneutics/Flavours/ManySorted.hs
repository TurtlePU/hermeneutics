module Hermeneutics.Flavours.ManySorted where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Flavours (HFoldable (..), HFunctor (..), HTraversable (..))

newtype SApp v s = MkSApp { runSApp :: v (s :| '[]) }

newtype ManySorted g v s = MSorted { runMSorted :: g (SApp v) s }

instance HFunctor g => HFunctor (ManySorted g) where
    hmap f = MSorted . hmap (MkSApp . f . runSApp) . runMSorted

instance HFoldable g => HFoldable (ManySorted g) where
    hfoldMap f = hfoldMap (f . runSApp) . runMSorted

instance HTraversable g => HTraversable (ManySorted g) where
    htraverse f =
        fmap MSorted . htraverse (fmap MkSApp . f . runSApp) . runMSorted
