{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BlockArguments #-}

module Hermeneutics.Multisorted.Grammar where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Grammar (Grammar (..))
import GHC.Generics ((:+:) (..), V1)

--------------------------------------------------------------------------------

-- multisorted without bindings

data AlgSort = Const | Algebra

data Example s v where
    (:.*:) :: v Const -> v Algebra -> Example Algebra v
    (:**:) :: v s -> v s -> Example s v
    (:++:) :: v s -> v s -> Example s v

instance Grammar (Example s) where
    gmap f (c :.*: a) = f c :.*: f a
    gmap f (a :**: b) = f a :**: f b
    gmap f (a :++: b) = f a :++: f b

newtype Flip p a b = MkFlip { runFlip :: p b a }

data Tree g s v where
    Leaf :: v s -> Tree g s v
    Node :: g s (Flip (Tree g) v) -> Tree g s v

(=>>) ::
    (forall t. Grammar (g t)) =>
    Tree g s u -> (forall t. u t -> Tree g t v) -> Tree g s v
Leaf v =>> f = f v
Node v =>> f = Node (gmap (MkFlip . (=>> f) . runFlip) v)

--------------------------------------------------------------------------------

-- multisorted with bindings

data Sort = Term | Type

-- a-la free foil (doesn't fit into framework, cringe)
data STLC s sc tm where
    Unit :: STLC Type sc tm
    Arr :: tm Type -> tm Type -> STLC Type sc tm
    So :: STLC Term sc tm
    App :: tm Term -> tm Term -> STLC Term sc tm
    Abs :: tm Type -> sc Term Term -> STLC Term sc tm

-- generalized funny stuff
data Lam s v where
    Un :: Lam Type v
    Ar :: v (Type :| '[]) -> v (Type :| '[]) -> Lam Type v

    S :: Lam Term v
    Ap :: v (Term :| '[]) -> v (Term :| '[]) -> Lam Term v
    Ab :: v (Type :| '[]) -> v (Term :| '[Term]) -> Lam Term v

instance Grammar (Lam s) where
    gmap _ Un = Un
    gmap f (Ar t u) = Ar (f t) (f u)
    gmap _ S = S
    gmap f (Ap t u) = Ap (f t) (f u)
    gmap f (Ab t b) = Ab (f t) (f b)

data In ts t where
    Here :: In (t : ts) t
    There :: In ts t -> In (u : ts) t

data Ext m v ts where
    MkExt :: m s (In ts :+: v) -> Ext m v (s :| ts)

-- note similarity with Tree above
data Scoped g s v where
    SLeaf :: v s -> Scoped g s v
    SNode :: g s (Ext (Scoped g) v) -> Scoped g s v

-- note similarity with =>> above
bind ::
    (forall s. Grammar (g s)) =>
    Scoped g t u -> (forall s. u s -> Scoped g s v) -> Scoped g t v
bind (SLeaf v) f = f v
bind (SNode n) f = SNode $ gmap (\(MkExt s) -> MkExt $ bind s \case
    { L1 i -> SLeaf (L1 i)
    ; R1 v -> bind (f v) (SLeaf . R1)
    }) n

data OnlyTerm s where OT :: OnlyTerm Term

-- an example of well-scoped lambda term in empty context.
closed :: Scoped Lam Term V1
closed = SLeaf OT `bind` \OT -> SNode (MkExt (SNode S) `Ap` MkExt (SNode S))
