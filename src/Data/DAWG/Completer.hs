{-|
Module: Data.DAWG.Completer
Description: Exports Completer API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.Completer
  ( Completer (..)
  , new
  , start
  , next
  , keyToString
  , value
  , completeKeys
  ) where

import Data.DAWG.Internal.Completer
