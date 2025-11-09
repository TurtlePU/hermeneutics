{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Hermeneutics.Parser where

import Data.Foldable (traverse_)
import Data.Graph qualified as G
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as N
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Recursive.DualBool qualified as RDB
import Data.Set (Set)
import Data.Set qualified as S
import qualified Data.Recursive.Set as RS

newtype Production s t = Production { pSeq :: [Either s t] }
  deriving (Eq, Ord)

data CFG s t = CFG { cfgRoot :: s, cfgRules :: Map s [Production s t] }

newtype Cycle s = Cycle { vertices :: NonEmpty s }

data NotLL1 s t
  = LeftRecursive (NonEmpty (Cycle s))
  | Ambiguous s (Maybe t) (NonEmpty (Production s t))

newtype LL1Table s t = LL1Table
  { tableLL1 :: Map (s, Maybe t) (Production s t) }

data First t = First { firstTerms :: !(Set t), firstEps :: !Bool }

singleFirst :: t -> First t
singleFirst = (`First` False) . S.singleton

instance Ord t => Num (First t) where
  First t e + First t' e' = First (t <> t') (e || e')

  First t True * First t' e = First (t <> t') e
  f@(First _ False) * _ = f

  fromInteger n = First S.empty (n > 0)

  negate = id
  abs = id
  signum = const 1

parseTableLL1 :: (Ord s, Ord t) => CFG s t -> Either (NotLL1 s t) (LL1Table s t)
parseTableLL1 (CFG root rules) = do

  let hasEps = RDB.or . map (RDB.and . map atomHasEps . pSeq) <$> rules
      atomHasEps a = case a of
        Left s  -> fromMaybe RDB.false (hasEps M.!? s)
        Right _ -> RDB.false

  let epsChildren (pSeq -> as) =
        mapMaybe (either Just (const Nothing) . fst)
        $ takeWhile snd
        $ zip as (True : map (RDB.get . atomHasEps) as)

  traverse_ (Left . LeftRecursive)
    $ N.nonEmpty
    $ mapMaybe (\case G.NECyclicSCC vs -> Just (Cycle vs); _ -> Nothing)
    $ G.stronglyConnComp
    $ map (uncurry \s -> (s, s,) . concatMap epsChildren)
    $ M.toList rules

  let firstNT = sum . map N.head <$> firstPs
      firstPs = map (N.scanr ((*) . toFirst) 1 . pSeq) <$> rules
      toFirst = either (sum . flip M.lookup firstNT) singleFirst

  let follows0 = M.fromListWith (<>)
        $ (root, (S.singleton Nothing, S.empty)) : [
          (s, (S.mapMonotonic Just t, S.fromList [s' | e]))
          | (ps, (s', fss)) <- zip (M.elems rules) (M.assocs firstPs)
          , (p, fs) <- zip ps fss
          , (Left s, First t e) <- zip (pSeq p) (N.tail fs)
        ]
      followsR = (\(f, ss) -> RS.mk f `RS.union` followssR ss) <$> follows0
      followssR = RS.unions . mapMaybe (followsR M.!?) . S.toList

  fmap LL1Table
    $ M.traverseWithKey (\(s, t) -> \case p N.:| [] -> Right p
                                          ps -> Left (Ambiguous s t ps))
    $ M.fromListWith (<>)
    $ [ ((s, t), N.singleton p)
      | (ps, (s, fss)) <- zip (M.elems rules) (M.assocs firstPs)
      , (p, First ts e) <- zip ps (map N.head fss)
      , t <- S.toList (S.mapMonotonic Just ts
                    <> if e then RS.get (followsR M.! s) else S.empty)
      ]
