{-|
Module: Data.DAWG.RankedGuide
Description: Exports RankedGuide API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.RankedGuide
  ( -- * RankedGuide
    -- $doc

    -- ** Building
    -- $usage

    RankedGuide (..)
  , build
  , build'

    -- ** Basic operations

  , child
  , sibling
  , rankedGuideDictionary

    -- ** Loading and saving

  , read
  , write
  ) where

import Data.DAWG.Internal.RankedGuideBuilder (build, build')
import Data.DAWG.Internal.RankedGuide
  (RankedGuide (..), child, sibling, rankedGuideDictionary, read, write)

import Prelude hiding (read)

-- $doc
--
-- This module offers RankedGuide.

-- $usage
--
-- RankedGuide could be built from 'Data.DAWG.DAWG.DAWG' and 'Data.DAWG.Dictionary.Dictionary':
--
-- >>> guide <- build' dawg dict
--
-- If build failed, error will be raised. Alternatively, use 'build'.
--
-- Once guide is ready it could be saved locally:
--
-- >>> write "myRankedGuide.dawg" guide
--
-- And later loaded back:
--
-- >>> guide <- read "myRankedGuide.dawg"
--
