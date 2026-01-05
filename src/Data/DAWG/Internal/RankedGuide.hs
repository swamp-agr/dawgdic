{-|
Module: Data.DAWG.Internal.RankedGuide
Description: Exports ranked completer as well as its internal API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.Internal.RankedGuide where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import GHC.Generics

import Data.DAWG.Internal.BaseType
import Data.DAWG.Internal.Dictionary
import Data.DAWG.Internal.Guide (Guide (..))

import qualified Data.Binary as Binary

import qualified Data.DAWG.Internal.Guide as G

-- ** Ranked Guide


-- | Another kind of 'Data.DAWG.Internal.Guide.Guide' that provides efficient way to perform completions ranked by values that were stored in the Dictionary.
-- It has the same layout as 'Data.DAWG.Internal.Guide.Guide'.
newtype RankedGuide = RankedGuide { rankedGuide :: Guide }
  deriving newtype (Binary, NFData)
  deriving stock (Generic)

-- | Gets a child by given ranked guide index.
child :: BaseType -> RankedGuide -> UCharType
child !ix = G.child ix . rankedGuide

-- | Gets a sibling by given ranked guide index.
sibling :: BaseType -> RankedGuide -> UCharType
sibling !ix = G.sibling ix . rankedGuide

-- | ^ 'Data.DAWG.Internal.Dictionary.Dictionary' associated with this ranked guide.
rankedGuideDictionary :: RankedGuide -> Dictionary
rankedGuideDictionary = guideDictionary . rankedGuide

-- | Load ranked guide from a file.
read :: FilePath -> IO RankedGuide
read = Binary.decodeFile

-- | Save ranked guide to a file.
write :: FilePath -> RankedGuide -> IO ()
write = Binary.encodeFile

-- | Dump ranked guide to stdout.
dump :: RankedGuide -> IO ()
dump = G.dump . rankedGuide
