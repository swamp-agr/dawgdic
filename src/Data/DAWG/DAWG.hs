{-|
Module: Data.DAWG.DAWG
Description: Exports DAWG API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.DAWG
  ( -- * DAWG
    -- $doc
    DAWG(..)

    -- ** Building DAWG
    -- $usage

  , new
  , insert
  , insertWithLength
  , freeze
  , fromAscList
  -- ** Helpers
  , root
  , empty
  , child
  , sibling
  , value
  , isLeaf
  , label
  , isMerging
  , size
  ) where

import Data.DAWG.Internal.DAWGBuilder (new, insert, insertWithLength, freeze, fromAscList)
import Data.DAWG.Internal.DAWG

-- $doc
--
-- This module offers DAWG.

-- $usage
--
-- To build DAWG from sorted list of words, ignoring all words that could not be inserted due to cycles, e.g. "banana", use 'fromAscList':
--
-- >>> import Data.DAWG.DAWG
-- >>> dawg <- fromAscList . lines =<< readFile "/path/to/lexicon"
-- >>>
--
-- To get more control over inserting (i.e. inspecting insertion results), use following sequence:
--
-- >>> dawgBuilder <- new
-- >>>
-- >>> :{
--   forM_ content \(word, value) -> do
--     result <- insert word (Just value) dawgBuilder
--     unless result $ error "Insert failed"
-- >>>  :}
-- >>> dawg <- freeze dawgBuilder
-- >>>
