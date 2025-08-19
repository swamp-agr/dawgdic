{-|
Module: Data.DAWG.Dictionary
Description: Exports Dictionary API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.Dictionary
  ( -- * Dictionary
    -- $doc
    Dictionary(..)

    -- ** Building
    -- $usage

  , build
  , build'
  , freeze

    -- ** Basic operations

  , root
  , hasValue
  , value
  , contains
  , containsPrefixLength
  , member
  , lookup
  , lookupPrefixLength
  , followChar
  , follow
  , followPrefixLength

    -- ** Loading and saving

  , read
  , write
  ) where

import Data.DAWG.Internal.DictionaryBuilder (build, build', freeze)
import Data.DAWG.Internal.Dictionary
  ( Dictionary (..)
  , root
  , hasValue
  , value
  , contains
  , containsPrefixLength
  , member
  , lookup
  , lookupPrefixLength
  , followChar
  , follow
  , followPrefixLength
  , read
  , write
  )
import Prelude hiding (read, lookup)

-- $doc
--
-- This module offers Dictionary.

-- $usage
--
-- Dictionary could be built from 'Data.DAWG.DAWG.DAWG':
--
-- >>> dict <- build' dawg
--
-- If build failed, error will be raised.
-- Alternatively, use combination of 'build' and 'freeze'.
--
-- Once dictionary is ready it could be saved locally:
--
-- >>> write "myDictionary.dawg" dict
--
-- And later loaded back:
--
-- >>> dict <- read "myDictionary.dawg"
--
