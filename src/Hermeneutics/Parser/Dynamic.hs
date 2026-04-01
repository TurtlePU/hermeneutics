{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Hermeneutics.Parser.Dynamic
-- Description : An implementation of resilient LL(1) parser
--               with dynamically typed parse tree.
-- Copyright   : (c) TurtlePU, 2025
-- License     : BSD-3
-- Maintainer  : sokolov.p64@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains a definition of a "dynamically typed" parse tree as well
-- as an algorithm to retrieve this parse tree from the stream of tokens
-- given an LL(1) jump table from 'Hermeneutics.Parser.Generator'.
module Hermeneutics.Parser.Dynamic where

import Control.Monad.Trans.State (gets, runState, state)
import Data.Bifunctor (first)
import Data.List (uncons)
import Data.Map qualified as M
import Data.Maybe (isJust, listToMaybe)
import Data.Traversable (for)
import Hermeneutics.Parser.Generator

-- | During LL(1) parsing, error recovery might either:
--
-- * Skip unrecognized ('Unexpected') tokens in a stream;
-- * Or "insert" missing tokens into stream ('Inserted').
data ErrorTokenKind = Unexpected | Inserted

-- | Result of dynamically typed parsing is a 'DynamicParseTree'.
data DynamicParseTree s t i
  -- | Leaves of a dynamic parse tree are individual tokens (or error tokens).
  = Terminal (Either (ErrorTokenKind, t) t)
  -- | Nodes of a dynamic parse tree are applications of rules
  -- (or error nonterminals).
  | NonTerminal (Either s i) [DynamicParseTree s t i]

-- | Given an efficient jump table and a token stream,
-- build the corresponding parse tree and return remaining tokens, if any.
parseDynamicLL1 ::
  (Ord s, Ord t) => LL1Table s t i -> [t] -> (DynamicParseTree s t i, [t])
parseDynamicLL1 (MkLL1Table root table) = runState (go root)
 where
  go si = do
    unrecognized <- state $ break (isJust . (table M.!?) . (si,) . Just)
    token <- gets listToMaybe
    case table M.!? (si, token) of
      (Nothing; Just Recover) -> pure $
        NonTerminal (Left si) (Terminal . Left . (Unexpected,) <$> unrecognized)
      Just (Apply MkProduction {..}) ->
        NonTerminal (Right pInfo) <$> for pSeq \case
          Left newState -> go newState
          Right terminal -> do
            nextToken <- state $ maybe (Nothing, []) (first Just) . uncons
            pure $ Terminal if nextToken == Just terminal then Right terminal
                            else Left (Inserted, terminal)
