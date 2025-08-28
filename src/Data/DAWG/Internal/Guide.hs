{-|
Module: Data.DAWG.Internal.Guide
Description: Exports guide as well as its internal API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.Internal.Guide where

import Control.DeepSeq (NFData)
import Control.Monad (forM_)
import Data.Binary
import Data.Vector.Unboxed (Vector)
import Data.Vector.Binary ()
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import Data.DAWG.Internal.BaseType
import Data.DAWG.Internal.GuideUnit (GuideUnit)

import qualified Data.Binary as Binary
import qualified Data.Vector.Unboxed as Vector

import qualified Data.DAWG.Internal.GuideUnit as GuideUnit

-- ** Guide

-- | Guide. Together with 'Data.DAWG.Internal.Dictionary.Dictionary'
-- it provides efficient way to look word prefixes up (completion requests).
--
-- * Each unit is stored in 2 bytes.
-- * Guide size is stored in unsigned int.
--
data Guide = Guide
  { guideUnits :: Vector GuideUnit -- ^ Array of 'Data.DAWG.Internal.GuideUnit.GuideUnit'. Index is equal to 'Data.DAWG.Internal.Dictionary.Dictionary' index.
  , guideSize :: SizeType -- ^ Size of array. Stored separately.
  } deriving (Generic, Binary, NFData)

-- | Constructs an empty guide.
empty :: Guide
empty = Guide
  { guideUnits = Vector.empty
  , guideSize = 0
  }
{-# INLINE empty #-}

-- | Root guide index. Equivalent to @0@.
root :: BaseType
root = 0
{-# INLINE root #-}

-- | Gets a child by given guide index.
child :: HasCallStack => BaseType -> Guide -> UCharType
child !ix !g = GuideUnit.child (guideUnits g Vector.! fromIntegral ix)
{-# INLINE child #-}

-- | Gets a sibling by given guide index.
sibling :: HasCallStack => BaseType -> Guide -> UCharType
sibling !ix !g = GuideUnit.sibling (guideUnits g Vector.! fromIntegral ix)
{-# INLINE sibling #-}

-- | Load guide from a file.
read :: HasCallStack => FilePath -> IO Guide
read = Binary.decodeFile

-- | Save guide to a file.
write :: HasCallStack => FilePath -> Guide -> IO ()
write = Binary.encodeFile

-- | Dump guide to stdout.
dump :: HasCallStack => Guide -> IO ()
dump Guide{..} = do
  let gsize = Vector.length guideUnits
  putStrLn $ concat ["guide\t(", show gsize, ")"]
  forM_ [0 .. gsize - 1] \ix -> do
    putStrLn $ concat
      [show ix, "\t", show $ guideUnits Vector.! fromIntegral ix]
