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
import Prelude hiding (read, lookup)
