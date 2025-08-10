{-|
Module: Data.DAWG.DAWG
Description: Exports DAWG API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.DAWG
  ( DAWG(..)
  , new
  , insert
  , insertWithLength
  , freeze
  , fromAscList
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
