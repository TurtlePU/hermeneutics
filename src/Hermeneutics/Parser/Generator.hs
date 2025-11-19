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
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Recursive.DualBool (RDualBool)
import Data.Recursive.DualBool qualified as RDB
import Data.Recursive.Set qualified as RS
import Data.Set (Set)
import Data.Set qualified as S

-- | A single @'Production' s t i@ in a 'CFG' (context-free grammar) is a
-- sequence 'pSeq' of nonterminals @s@ and terminals @t@.
-- Additional information of type @i@ can be stored in a field 'pInfo'.
data Production s t i = MkProduction { pSeq :: [Either s t], pInfo :: i }

-- | A @'CFG' s t i@ (context-free grammar) is a mapping of 'cfgRules' where
-- each rule associates a nonterminal @s@ with its possible productions
-- typed as @'Production' s t i@.
-- 'cfgRoot' nonterminal corresponds to the language recognized by the grammar.
data CFG s t i = MkCFG { cfgRoot :: s, cfgRules :: Map s [Production s t i] }

-- | A @'Cycle' s@ is a non-empty list of 'vertices' @s@.
newtype Cycle s = MkCycle { vertices :: NonEmpty s }

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

-- | LL(1) parser can take three possible actions on input:
-- apply a production rule, enter recovery mode or skip unknown token.
data Action s t i = Apply (Production s t i) | Recover

-- | A jump table for LL(1) grammar @'LL1Table' s t i@ is a mapping that tells
-- which @'Action' s t i@ to take at each possible combination
-- of nonterminal state @s@ and input symbol @t@ (or end of input).
data LL1Table s t i = MkLL1Table
  { tableRoot :: s, tableLL1 :: Map (s, Maybe t) (Action s t i) }

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
--
-- * "alt", joining 'First' sets of two productions of a same nonterminal;
-- * "seq", computing a 'First' set of concatenation of two productions.
--
-- If properly indexed by the resulting nonterminal as a type, @'First' t@ would
-- form a 'Control.Applicative.Alternative' functor under these operations,
-- but here we just give it a 'Num' instance with the following properties:
--
-- * @('First' t, '+', 0)@ is an abelian monoid (where '+' is "alt");
-- * @('First' t, '*', 1)@ is a monoid (where '*' is "seq");
-- * Multiplication ditributes over addition;
-- * @0 * ff = 0@;
-- * @(ts, _) * 0 = (ts, False)@ where @ts@ is a set of terminals.
--
-- So it's /almost/ like a semiring, but multiplication is a bit more eager.
data First t = MkFirst { firstTerms :: Set t, firstEps :: RDualBool }

-- | @'First' t@ might contain a single terminal @t@.
singleFirst :: t -> First t
singleFirst = (`MkFirst` RDB.false) . S.singleton

instance Ord t => Num (First t) where
  ~(MkFirst t e) + ~(MkFirst t' e') = MkFirst (t <> t') (e RDB.|| e')
  ~(MkFirst t e) * ~(MkFirst t' e') =
    MkFirst (t <> if RDB.get e then t' else S.empty) (e RDB.&& e')
  fromInteger = MkFirst S.empty . RDB.mk . (> 0)
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
parseTableLL1 (MkCFG root rules) = do

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
    $ mapMaybe (\case G.NECyclicSCC vs -> Just (MkCycle vs); _ -> Nothing)
    $ G.stronglyConnComp
    $ map (uncurry \s -> (s, s,) . concatMap epsChildren)
    $ M.toList rules

  let follows = M.fromListWith RS.union
        $ (root, RS.singleton Nothing) : [
          (s, RS.mk (S.mapMonotonic Just t) `RS.union` f')
          | (ps, (s', fss)) <- zip (M.elems rules) (M.assocs firstPs)
          , (p, fs) <- zip ps fss
          , (Left s, MkFirst t e) <- zip (pSeq p) (N.tail fs)
          , let f' = fromMaybe RS.empty (guard (RDB.get e) >> follows M.!? s')
        ]

  let accumR (Left ((firstNT M.!?) -> Just (MkFirst f _))) = RS.union (RS.mk f)
      accumR (Right t)                                     = RS.insert t
      accumR _                                             = id
      recovery = M.fromListWith RS.union
        [ (s, r)
        | (s', ps) <- M.assocs rules
        , p <- map pSeq ps
        , let rs = N.scanr accumR (fromMaybe RS.empty $ recovery M.!? s') p
        , (Left s, r) <- zip p (N.tail rs)
        ]
      recoveries =
        [ ((s, Just t), [])
        | (s, ts) <- M.assocs recovery
        , t <- S.toList (RS.get ts)
        ]

  fmap (MkLL1Table root)
    $ M.traverseWithKey (\(s, t) -> \case
        [] -> Right Recover
        [p] -> Right (Apply p)
        (p : ps) -> Left $ Ambiguous s t (p N.:| ps)
      )
    $ M.fromListWith (<>)
    $ [ ((s, t), [p])
      | (ps, (s, fss)) <- zip (M.elems rules) (M.assocs firstPs)
      , (p, MkFirst ts e) <- zip ps (map N.head fss)
      , t <- S.toList (S.mapMonotonic Just ts
                    <> if RDB.get e then RS.get (follows M.! s) else S.empty)
      ] ++ recoveries
