-- |
-- Module      : Hermeneutics.Encoding.Source
-- Description : A language of "source" terms in a given grammar.
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains a definition of a type of "source" terms generated
-- from the given many-sorted grammar with bindings (consult
-- "Hermeneutics.Grammar" for more info on what this means), meaning that
-- variables are uniformly represented with a generic "text" type.
module Hermeneutics.Encoding.Source where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Grammar (HFunctor, hmap)

-- | Given a representation of each sort, represents
-- list of sorts by heterogeneous list of representations.
data Intros t ss where
    INil :: Intros t '[]
    ICons :: t s -> Intros t ss -> Intros t (s : ss)

instance HFunctor Intros where
    hmap _ INil         = INil
    hmap f (ICons i is) = f i `ICons` hmap f is

-- | A source term @m@ with bindings from @ss@ represented by @t@.
data ExtSource m t ss where
    ESource :: Intros t ss -> m t s -> ExtSource m t (s :| ss)

instance HFunctor m => HFunctor (ExtSource m) where
    hmap f (ESource i s) = hmap f i `ESource` hmap f s

-- | A source term representation where
-- * @g@ is grammar,
-- * @t@ is representation of variables for each sort,
-- * @s@ is target sort of a term.
data Source g t s = SVar (t s) | SNode (g (ExtSource (Source g) t) s)

instance HFunctor g => HFunctor (Source g) where
    hmap f = go
      where
        go (SVar t)  = SVar (f t)
        go (SNode n) = SNode $ hmap (hmap f) n
