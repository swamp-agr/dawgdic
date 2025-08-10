{-|
Module: Data.DAWG.Guide
Description: Exports Guide API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.Guide
  ( Guide (..)
  , build
  , build'
  , empty
  , child
  , sibling
  , read
  , write
  ) where

import Data.DAWG.Internal.GuideBuilder (build, build')
import Data.DAWG.Internal.Guide (Guide (..), empty, child, sibling, read, write)

import Prelude hiding (read)
