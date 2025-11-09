{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Hermeneutics.Parser.Generator
-- Description : An implementation of resilient LL(1) parser generator.
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains a definition of a "context-free grammar" datatype as
-- well as an algorithm to turn a valid LL(1) grammar into efficient jump table.
module Hermeneutics.Parser.Generator where

import Control.Monad (guard)
import Data.Foldable (traverse_)
import Data.Graph qualified as G
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as N
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Recursive.DualBool (RDualBool)
import Data.Recursive.DualBool qualified as RDB
import Data.Recursive.Set qualified as RS
import Data.Set (Set)
import Data.Set qualified as S

-- | A single @'Production' s t i@ in a 'CFG' (context-free grammar) is a
-- sequence 'pSeq' of nonterminals @s@ and terminals @t@.
-- Additional information of type @i@ can be stored in a field 'pInfo'.
data Production s t i = Production { pSeq :: [Either s t], pInfo :: i }

-- | A @'CFG' s t i@ (context-free grammar) is a mapping of 'cfgRules' where
-- each rule associates a nonterminal @s@ with its possible productions
-- typed as @'Production' s t i@.
-- 'cfgRoot' nonterminal corresponds to the language recognized by the grammar.
data CFG s t i = CFG { cfgRoot :: s, cfgRules :: Map s [Production s t i] }

-- | A @'Cycle' s@ is a non-empty list of 'vertices' @s@.
newtype Cycle s = Cycle { vertices :: NonEmpty s }

-- | @'NotLL1' s t i@ is an enumeration of possible reasons
-- why a particular @'CFG' s t i@ (context-free-grammar)
-- might not be accepted by our parser generator.
data NotLL1 s t i
  -- | Grammar is left-recursive if there is a cycle of productions
  -- which never consumes a single token.
  = LeftRecursive (NonEmpty (Cycle s))
  -- | Grammar is ambiguous if there are many possible productions to apply
  -- at a given nonterminal state @s@ and input symbol @t@ (or end of input).
  | Ambiguous s (Maybe t) (NonEmpty (Production s t i))

-- | A jump table for LL(1) grammar @'LL1Table' s t i@ is a mapping that tells
-- which @'Production' s t i@ to apply at each possible combination
-- of nonterminal state @s@ and input symbol @t@ (or end of input).
newtype LL1Table s t i = LL1Table
  { tableLL1 :: Map (s, Maybe t) (Production s t i) }

-- | A @'First' t@ set of an arbitrary production (/p/)
-- is a set of terminals @t@ which might start a sentence recognized by (/p/).
-- It also includes empty string if (/p/) might descend into a nonterminal
-- by not consuming a single terminal token.
--
-- __Note__: includedness of empty string is computed with recursive booleans
-- from the [rec-def](https://hackage.haskell.org/package/rec-def) package.
-- Thanks to additional laziness introduced by @rec-def@,
-- we can compute this field for arbitrary context-free grammars,
-- enabling easy validity checks.
--
-- It was
-- [noted by Swierstra and Duponcheel](https://www.cs.tufts.edu/comp/150FP/archive/doaitse-swierstra/error-correcting.pdf)
-- that 'First' sets have two basic operations:
-- * "alt", joining 'First' sets of two productions of a same nonterminal;
-- * "seq", computing a 'First' set of concatenation of two productions.
--
-- If properly indexed by the resulting nonterminal as a type, @'First' t@ would
-- form a 'Control.Applicative.Alternative' functor under these operations,
-- but in our case we just give it a 'Num' instance with the following properties:
-- * @('First' t, '+', 0)@ is an abelian monoid (where '+' is "alt");
-- * @('First' t, '*', 1)@ is a monoid (where '*' is "seq");
-- * Multiplication ditributes over addition;
-- * @0 * ff = 0@;
-- * @(ts, _) * 0 = (ts, False)@ where @ts@ is a set of terminals.
-- So it's /almost/ like a semiring, but multiplication is a bit more eager.
data First t = First { firstTerms :: Set t, firstEps :: RDualBool }

-- | @'First' t@ might contain a single terminal @t@.
singleFirst :: t -> First t
singleFirst = (`First` RDB.false) . S.singleton

instance Ord t => Num (First t) where
  First t e + First t' e' = First (t <> t') (e RDB.|| e')
  First t e * First t' e' =
    First (t <> if RDB.get e then t' else S.empty) (e RDB.&& e')
  fromInteger = First S.empty . RDB.mk . (> 0)
  negate = id
  abs = id
  signum = const 1

-- | Given a context-free grammar @g :: 'CFG' s t i@
-- over nonterminals @s@, terminal tokens @t@ and production annotations @i@,
-- @'parseTableLL1' g@ is either
-- a 'Left' explaining why @g@ isn't an LL(1) grammar,
-- or 'Right' containing an efficient jump table for @g@.
parseTableLL1 ::
  (Ord s, Ord t) => CFG s t i -> Either (NotLL1 s t i) (LL1Table s t i)
parseTableLL1 (CFG root rules) = do

  let firstNT = sum . map N.head <$> firstPs
      firstPs = map (N.scanr ((*) . toFirst) 1 . pSeq) <$> rules
      toFirst = either (sum . flip M.lookup firstNT) singleFirst

  let atomEps a = case a of
        Left s  -> maybe False (RDB.get . firstEps) (firstNT M.!? s)
        Right _ -> False
      epsChildren (pSeq -> as) =
        mapMaybe (either Just (const Nothing) . fst)
        $ takeWhile snd
        $ zip as (True : map atomEps as)

  traverse_ (Left . LeftRecursive)
    $ N.nonEmpty
    $ mapMaybe (\case G.NECyclicSCC vs -> Just (Cycle vs); _ -> Nothing)
    $ G.stronglyConnComp
    $ map (uncurry \s -> (s, s,) . concatMap epsChildren)
    $ M.toList rules

  let follows = M.fromListWith RS.union
        $ (root, RS.singleton Nothing) : [
          (s, RS.mk (S.mapMonotonic Just t) `RS.union` f')
          | (ps, (s', fss)) <- zip (M.elems rules) (M.assocs firstPs)
          , (p, fs) <- zip ps fss
          , (Left s, First t e) <- zip (pSeq p) (N.tail fs)
          , let f' = fromMaybe RS.empty (guard (RDB.get e) >> follows M.!? s')
        ]

  fmap LL1Table
    $ M.traverseWithKey (\(s, t) -> \case p N.:| [] -> Right p
                                          ps -> Left (Ambiguous s t ps))
    $ M.fromListWith (<>)
    $ [ ((s, t), N.singleton p)
      | (ps, (s, fss)) <- zip (M.elems rules) (M.assocs firstPs)
      , (p, First ts e) <- zip ps (map N.head fss)
      , t <- S.toList (S.mapMonotonic Just ts
                    <> if RDB.get e then RS.get (follows M.! s) else S.empty)
      ]
