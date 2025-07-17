-- |
-- Module      : Hermeneutics.Flavours
-- Description : Definition of grammars as higher-order functors. Start here!
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains definition of 'HFunctor', 'HFoldable' and 'HTraversable'
-- classes, analogues of 'Functor', 'Foldable' and 'Traversable' lifted to the
-- category of functors.
--
-- Turns out that 'HFunctor's can be viewed as grammars defining languages with
-- variable bindings in the same way 'Functor's can be viewed as algebraic
-- signatures. This is the most general case; for more specific usecases, there
-- are adapters available, defined in Hermeneutics.Flavours.* modules. Consult
-- the table below for more information.
--
-- = Grammar classification
--
-- Formal languages, being some arbitrary subsets of the set of all possible
-- texts, can be defined in a plethora of ways. The most well-accepted one is
-- via /grammars/, a set of rules which can be repeatedly applied to generate
-- texts in a language. In our usecase of working with programming languages,
-- context-free grammars (CFGs) are most useful: the rules in a CFG are
-- structured in such a way that each text in a generated language can be parsed
-- back into a /syntax tree/ where nodes correspond to the rules which were used
-- to generate the substring which corresponds to the node.
--
-- In involved programming languages, there might be many different kinds of
-- things which are generated from different grammars. For example, in a typed
-- language of expressions, we already have two such kinds of things: value
-- expressions and type expressions. In this library, we will call these kinds
-- of things /sorts/.
--
-- CFGs, however, are not all there is to the syntax of programming languages:
-- the second (or, arguably, the first) most important aspect is a notion of
-- /variable binding/. A good programming language allows its user to define new
-- things and to name these things to later refer to them by their name.
--
-- Therefore, we can classify grammars along two axes:
--
--     1. How many sorts it has;
--     2. Does it support variable bindings and, if yes, how many.
--
-- We can summarize it in a table:
--
-- +---------------+--------------+-------------+
-- | # of bindings |    1 sort    |  Many sorts |
-- +===============+==============+=============+
-- |  No bindings  |  Functorial  |  Algebraic  |
-- +---------------+--------------+-------------+
-- |  1 at a time  | Second-order | Categorical |
-- +---------------+--------------+-------------+
-- |  n at a time  |   Nth-order  |     Full    |
-- +---------------+--------------+-------------+
--
-- (where /n at a time/ actually means /any finite amount at a time/)
--
-- == Functorial grammars
--
-- Simplest kind of grammars there is: only a single sort, no bindings.
-- Typically, this is a sort of grammars for languages of algebraic expressions,
-- like \(x * 8 + (3 - y) ^ 2\) (Note that we still can use variables, it's just
-- that we cannot create new ones). In case you read [What is algebraic about
-- algebraic effects and handlers?](https://arxiv.org/abs/1807.05923), you would
-- immediately recognize that such languages can be generated using your usual
-- Haskell 'Functor's, and a way to generate them is a
-- [Free monad](https://hackage.haskell.org/package/free).
--
-- We could stop there, but we need a more general solution. To aid
-- understanding, let us introduce
--
-- === Term providers
--
-- /Term/ is mostly synonymous with /expression/; and, to be precise, we're
-- going to introduce the notion of (sub)expression providers. These are the
-- entities which denote a subexpression in the rules of a grammar, hiding
-- unnecessary details. For example, in case of functorial grammar
-- @f :: Type -> Type@, applied to type @t :: Type@, yields @f t@. Here, @t@ is
-- precisely the term provider; in this simple case, it is just the stub for a
-- subexpression, but we will see more interesting examples next.
--
-- == Second-order grammars
--
-- To incorporate a notion of binding, there is a
-- [genious idea](https://arxiv.org/pdf/2204.05653) to use a Bifunctor
-- instead of a Functor. Parameters @s@ and @t@ of kind @Type@, fed into a
-- Bifunctor @b@, are its term providers; @s@ stands for @s@coped subterms,
-- and @t@ stands for usual sub@t@erms. So, if you wish for a subterm to work
-- under newly introduced variable binding, you use @s@; if you just wish to
-- have a new subterm without any variable bindings, you use @t@.
--
-- == Nth-order grammars
--
-- For single-sorted core languages which introduce a single binding at a time,
-- Bifunctors are perfectly fine. However, if you were to define a grammar for
-- user-facing language, you would most likely define language constructs which
-- introduce several new bindings simultaneously. To this end, we can generalize
-- an idea of second-order grammars: instead of having two term providers, let
-- us accept a countably infinite number of providers, where @n@-th term
-- provider would stand for a subterm with @n@ new variable bindings. It is best
-- presented as a function of kind @Natural -> Type@; therefore, the kind of an
-- N-th order grammar would correspondingly be @(Natural -> Type) -> Type@.
-- Module "Hermeneutics.Flavours.NthOrder" calls such functors DFunctors; as you
-- can see, these are just like Functors, but they map from the category of
-- functors instead of Hask. And, of course, there are DFoldable and
-- DTraversable too, and all of them can be derived via Generic1 (see the module
-- for more details).
--
-- == Algebraic grammars
--
-- You might think that functorial grammars would better be called algebraic
-- grammars (since they also deal with algebraic expressions), but we reserved
-- this name for many-sorted grammars without bindings for a simple reason. An
-- algebraic theory of Algebras (in the
-- [mathematical structure](https://en.wikipedia.org/wiki/Algebra_over_a_field)
-- sense) is actually two-sorted: there is a sort of constants and a sort of
-- algebra elements. In general, such structures are studied in a
-- /many-sorted logic/, so that's one more source of naming in this library.
--
-- So, how can we define a many-sorted grammar without bindings? Continuing
-- with our idea of term providers, for each sort @s@, there should be a
-- corresponding term provider @t s@. Moreover, a grammar itself should be
-- indexed by the sort as well, since we wish to provide a separate grammar for
-- each sort. So the kind is @(s -> Type) -> (s -> Type)@; as you can see, these
-- are functors as well, but one level higher: from category of functors to the
-- category of functors once again. Following
-- [convention](https://hackage.haskell.org/package/functor-combinators), we
-- call them 'HFunctor's; once again, there are their siblings called
-- 'HFoldable' and 'HTraversable'. That's not the last you've heard of them!
-- Keep reading.
--
-- == Full grammars
--
-- So, how do we get to defining a many-sorted grammar with bindings? As should
-- be clear from the section on algebraic grammars, a grammar should be indexed
-- by sort; there at least should be term providers for each sort. In addition,
-- for each new binding we introduce, we should be able to specify the sort of
-- the variable that's being introduced! So, for each /list/ of sorts for
-- bindings together with a sort of a subterm, there should be a corresponding
-- term provider. The kind of grammar is @(NonEmpty s -> Type) -> (s -> Type)@;
-- this is 'HFunctor' again! The difference is in the leftmost kind.
--
-- == Categorical grammars
--
-- To wrap up, we should meet one more interesting special case: many-sorted
-- grammar where bindings are introduced one-by-one. As is with second-order
-- syntax, we shall have two separate term provider families: one for subterms
-- without bindings; another for subterms together with a single new variable
-- binding. Remembering that the grammar is many-sorted, we arrive at the
-- following kinds for term providers: @s -> Type@ for unscoped and
-- @s -> s -> Type@ for scoped. The kind of grammar is quite involved:
--
-- @
--      (s -> s -> Type) -> (s -> Type) -> (s -> Type)
-- @
--
-- But the reasoning should be clear at this point. Once again, this looks like
-- a generalization of a Bifunctor; it's called a CFunctor and is defined in
-- "Hermeneutics.Flavours.Categorical" along with its siblings, CFoldable and
-- CTraversable.
--
-- (Note on the naming: it's unrelated to the actual study of
-- /categorical grammars/ in linguistics and mathematical logic. It's just that
-- the term providers look awful lot like sets of /objects/ and /hom-sets/ of
-- some category represented by @s@.)
--
-- == Unified representation
--
-- So we learned that, most generally, @s@-sorted grammar with arbitrary
-- variable bindings can be presented via datatype of a kind
--
-- @
--      (NonEmpty s -> Type) -> (s -> Type)
-- @
--
-- But there are a lot of different more specialized /flavours/ of grammars as
-- well, each presentable with their own kind of functors. Is there a way to
-- bind them all together?
--
-- Luckily, there is: a single-sorted grammar can be viewed as a special case of
-- many-sorted grammar with a single sort; a grammar without bindings can be
-- viewed as one with bindings, it's just that it never uses this possibility.
-- Thus it is enough to provide converters (called /adapters/ in this library)
-- which lift special-case functors to their most general counterpart. A short
-- table of contents to choose your favourite flavour:
--
-- +---------------+-------------------------------------+-------------------------------------+
-- | # of bindings |                1 sort               |               Many sorts            |
-- +===============+=====================================+=====================================+
-- |  No bindings  | "Hermeneutics.Flavours.FirstOrder"  | "Hermeneutics.Flavours.ManySorted"  |
-- +---------------+-------------------------------------+-------------------------------------+
-- |  1 at a time  | "Hermeneutics.Flavours.SecondOrder" | "Hermeneutics.Flavours.Categorical" |
-- +---------------+-------------------------------------+-------------------------------------+
-- |  n at a time  |  "Hermeneutics.Flavours.NthOrder"   |             This module             |
-- +---------------+-------------------------------------+-------------------------------------+
module Hermeneutics.Flavours where

import GHC.Generics ((:.:) (..))

type a ~> b = forall i. a i -> b i

type a /> b = forall i. a i -> b

type Klei f a b = forall i. a i -> f (b i)

class HFunctor f where
    hmap :: (a ~> b) -> (f a ~> f b)

instance Functor f => HFunctor ((:.:) f) where
    hmap f = Comp1 . fmap f . unComp1

class HFunctor m => HMonad m where
    hpure :: a ~> m a
    hbind :: (a ~> m b) -> (m a ~> m b)

class HFoldable f where
    hfoldMap :: Monoid m => (a /> m) -> f a /> m

class (HFunctor t, HFoldable t) => HTraversable t where
    htraverse :: Applicative f => Klei f a b -> Klei f (t a) (t b)
