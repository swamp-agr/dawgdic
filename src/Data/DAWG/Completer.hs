{-|
Module: Data.DAWG.Completer
Description: Exports Completer API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.Completer
  ( -- * Completer
    -- $usage
    Completer (..)
  , start
  , next
  , keyToString
  , value
  , completeKeys
  , keys
  , values
  , toList
  ) where

import Data.DAWG.Internal.Completer


-- $usage
--
-- Consider following lexicon:
--
-- @
--   an
--   and
--   appear
--   apple
--   bin
--   can
--   cat
-- @
--
-- 1. Build DAWG.
--
-- >>> import qualified Data.DAWG.DAWG as Dawg
-- >>> contents <- lines <$> readFile "lexicon.txt"
-- >>> dawg <- Dawg.build' contents
--
-- 2. Build Dictionary.
--
-- >>> import qualified Data.DAWG.Dictionary as Dict
-- >>> dict <- Dict.build' dawg
--
-- 3. Build Guide.
--
-- >>> import qualified Data.DAWG.Guide as G
-- >>> guide <- G.build' dawg dict
--
-- From now on it is possible to perform completion requests via 'completeKeys'.
-- To get more control over completion, consider using 'Completer' directly.
--
-- Start completion for @"a"@. First, let's find the dictionary index to start with.
--
-- >>> let Just dictIndex = Dict.followPrefixLength "a" 1 Dict.root dict
--
-- Begin completing. Prepare 'Completer' to traverse the dictionary using guide.
--
-- >>> let c_started = start dictIndex "a" dict guide
--
-- Get next completion result
--
-- >>> let mc_next = next c_started
-- >>> :t mc_next
-- Maybe Completer
--
-- When it is 'Nothing' there is nothing to complete in this dictionary.
--
-- >>> let Just c_next = mc_next
--
-- The completion, i.e. the remainder for the requested prefix is stored in 'completerKey'.
-- To retrieve it from 'Completer' use 'keyToString':
--
-- >>> completeKey c_next
-- "n"
--
-- To get the next completion result, run 'next' once more.
--
-- >>> let Just c_next1 = next c_next
-- >>> completeKey c_next1
-- "nd"
--
-- Consider lexicon where each word has associated value with it.
-- To obtain the value for the current completion, use 'value'.
--
-- >>> value c_next1
-- 0
--
-- @0@ is equivalent to empty value or its absence.
--
--
