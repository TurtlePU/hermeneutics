{-# LANGUAGE BlockArguments #-}

module Hermeneutics.Parser where

import Data.Map (Map)
import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Foldable (traverse_)
import qualified Data.Graph as G
import qualified Data.Map as M
import Control.Monad ((<=<))
import qualified Data.Set as S
import qualified Data.List.NonEmpty as N

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

  let firstIfLeft = either pure (const Nothing) <=< listToMaybe
  traverse_ (Left . LeftRecursive)
    $ N.nonEmpty
    $ mapMaybe (\case G.NECyclicSCC vs -> Just (Cycle vs); _ -> Nothing)
    $ G.stronglyConnComp
    $ map (uncurry \s -> (s, s,) . mapMaybe (firstIfLeft . pSeq)) -- account for eps-transitions as well
    $ M.toList rules

  let firstNT = sum . map N.head <$> firstPs
      firstPs = map (N.scanr ((*) . toFirst) 1 . pSeq) <$> rules
      toFirst = either (sum . flip M.lookup firstNT) singleFirst

  let populate fi = \case
        G.AcyclicSCC (fs, s, ss') ->
          M.insert s (fs <> S.unions [ fi M.! s' | s' <- ss' ]) fi
        G.NECyclicSCC ns ->
          let (fs, ss, ss') =
                foldMap (\(f, s, s') -> (f, S.singleton s, S.fromList s')) ns
              fs' = fs <> S.unions [ fi M.! s' | s' <- S.toList (ss' S.\\ ss) ]
           in M.fromSet (const fs') ss <> fi
      follows =
        foldl' populate M.empty
        $ G.stronglyConnCompR
        $ map (\(s, (f, ss')) -> (f, s, S.toList ss'))
        $ M.assocs
        $ M.fromListWith (<>)
        $ (root, (S.singleton Nothing, S.empty)) : [
          (s, (S.mapMonotonic Just t, S.fromList [s' | e]))
          | (ps, (s', fss)) <- zip (M.elems rules) (M.assocs firstPs)
          , (p, fs) <- zip ps fss
          , (Left s, First t e) <- zip (pSeq p) (N.tail fs)
        ]

  fmap LL1Table
    $ M.traverseWithKey (\(s, t) -> \case p N.:| [] -> Right p
                                          ps -> Left (Ambiguous s t ps))
    $ M.fromListWith (<>)
    $ [ ((s, t), N.singleton p)
      | (ps, (s, fss)) <- zip (M.elems rules) (M.assocs firstPs)
      , (p, First ts e) <- zip ps (map N.head fss)
      , t <- S.toList (S.mapMonotonic Just ts
                    <> if e then follows M.! s else S.empty)
      ]
