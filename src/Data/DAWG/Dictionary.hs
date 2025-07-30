module Data.DAWG.Dictionary
  ( Dictionary(..)
  , build
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

import Data.DAWG.Internal.DictionaryBuilder (build, freeze)
import Data.DAWG.Internal.Dictionary
import Prelude hiding (read, lookup)
