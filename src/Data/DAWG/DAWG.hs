module Data.DAWG.DAWG
  ( DAWG(..)
  , new
  , insert
  , insertWithLength
  , freeze
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

import Data.DAWG.Internal.DAWGBuilder (new, insert, insertWithLength, freeze)
import Data.DAWG.Internal.DAWG
