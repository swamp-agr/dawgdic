{-|
Module: Data.DAWG.Dictionary
Description: Exports Dictionary API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.Dictionary
  ( Dictionary(..)
  , build
  , build'
  , freeze
  , root
  , hasValue
  , value
  , contains
  , containsPrefixLength
  , lookup
  , lookupPrefixLength
  , followChar
  , follow
  , followPrefixLength
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
  , lookup
  , lookupPrefixLength
  , followChar
  , follow
  , followPrefixLength
  , read
  , write
  )
import Prelude hiding (read, lookup)
