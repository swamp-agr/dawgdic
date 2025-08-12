{-|
Module: Data.DAWG.Completer
Description: Exports Completer API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.Completer
  ( -- * Completer
    -- $usage
    Completer (..)
  , new
  , start
  , next
  , keyToString
  , value
  , completeKeys
  ) where

import Data.DAWG.Internal.Completer

-- $usage
--
-- The most basic scenario is to get all completions by given 'Data.DAWG.Dictionary.Dictionary' and 'Data.DAWG.Guide.Guide'. Consider following lexicon:
--
-- @
--   a
--   an
--   and
--   appear
--   apple
--   bin
--   can
--   cat
-- @
--
-- Once dictionary and guide are ready, call 'completeKeys':
--
-- >>> completeKeys "a" dict guide
-- ["a", "an", "and", "appear", "apple"]
--
