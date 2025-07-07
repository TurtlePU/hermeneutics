{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BlockArguments #-}

module Hermeneutics.Multisorted where

import Data.List.NonEmpty (NonEmpty (..))
import Hermeneutics.Grammar (Grammar (..))
import GHC.Generics ((:+:) (..), V1)

--------------------------------------------------------------------------------

data Tree t g v s where
    Leaf :: v s -> Tree t g v s
    Node :: g s (t (Tree t g) v) -> Tree t g v s

type f ~> g = forall a. f a -> g a

class Monad1 m where
    unit1 :: a ~> m a
    bind1 :: (a ~> m b) -> (m a ~> m b)

(=>>) :: Monad1 m => m a c -> (a ~> m b) -> m b c
m =>> f = bind1 f m

class Tensor1 t where
    lift1 :: Monad1 m => (a ~> m b) -> (t m a ~> t m b)

instance (Tensor1 t, forall ss. Grammar (g ss)) => Monad1 (Tree t g) where
    unit1 = Leaf
    bind1 f (Leaf u) = f u
    bind1 f (Node n) = Node (gmap (lift1 f) n)

--------------------------------------------------------------------------------

newtype Id2 f a b = MkId2 { runId2 :: f a b }

instance Tensor1 Id2 where
    lift1 f = MkId2 . bind1 f . runId2

-- | Multisorted grammars without bindings
type FreeS = Tree Id2

data AlgSort = Const | Algebra

data Example s v where
    (:.*:) :: v Const -> v Algebra -> Example Algebra v
    (:**:) :: v s -> v s -> Example s v
    (:++:) :: v s -> v s -> Example s v

instance Grammar (Example s) where
    gmap f (c :.*: a) = f c :.*: f a
    gmap f (a :**: b) = f a :**: f b
    gmap f (a :++: b) = f a :++: f b

type AlgTerms = FreeS Example

--------------------------------------------------------------------------------

data In ts t where
    Here :: In (t : ts) t
    There :: In ts t -> In (u : ts) t

data Ext m v ts where
    MkExt :: m (In ts :+: v) s -> Ext m v (s :| ts)

instance Tensor1 Ext where
    lift1 f (MkExt m) = MkExt $ m =>> \case
        L1 i -> unit1 (L1 i)
        R1 v -> f v =>> (unit1 . R1)

-- | Multisorted grammars with bindings
type Scoped = Tree Ext

data Sort = Term | Type

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

type STLC = Scoped Lam

data OnlyTerm s where OT :: OnlyTerm Term

-- an example of well-scoped lambda term in empty context.
closed :: STLC V1 Term
closed = Leaf OT =>> \OT -> Node (MkExt (Node S) `Ap` MkExt (Node S))
