{-|
Module: Data.DAWG.Guide
Description: Exports Guide API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.Guide
  ( -- * Guide
    -- $doc

    -- ** Building
    -- $usage

    Guide (..)
  , build
  , build'

    -- ** Basic operations

  , empty
  , child
  , sibling

    -- ** Loading and saving

  , read
  , write
  ) where

import Data.DAWG.Internal.GuideBuilder (build, build')
import Data.DAWG.Internal.Guide (Guide (..), empty, child, sibling, read, write)

import Prelude hiding (read)

-- $doc
--
-- This module offers Guide.

-- $usage
--
-- Guide could be built from 'Data.DAWG.DAWG.DAWG' and 'Data.DAWG.Dictionary.Dictionary':
--
-- >>> guide <- build' dawg dict
--
-- If build failed, error will be raised. Alternatively, use 'build'.
--
-- Once guide is ready it could be saved locally:
--
-- >>> write "myGuide.dawg" guide
--
-- And later loaded back:
--
-- >>> guide <- read "myGuide.dawg"
--
