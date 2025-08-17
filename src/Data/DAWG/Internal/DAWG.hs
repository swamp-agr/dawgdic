{-|
Module: Data.DAWG.Internal.DAWG
Description: Exports dawg as well as its internal API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.Internal.DAWG where

import Control.DeepSeq (NFData)
import Control.Monad (forM_)
import Data.Bit (Bit (..))
import Data.Char
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import Data.DAWG.Internal.BaseType
import Data.DAWG.Internal.BaseUnit (BaseUnit (..))

import qualified Data.Vector.Unboxed as UV

import qualified Data.DAWG.Internal.BaseUnit as BU


-- ** DAWG

-- | Represents directed acycclic word graph (DAWG), the core data type of this package.
-- Could be built from the input lexicon (list of words).
-- Mostly it is used to generate separate 'Data.DAWG.Internal.Dictionary.Dictionary'
-- (for querying words and searching associated values)
-- and 'Data.DAWG.Internal.Guide' (for faster completions).
--
data DAWG = DAWG
  { dawgBasePool :: BasePool -- ^ Pool of base units.
  , dawgLabelPool :: LabelPool -- ^ Pool of characters.
  , dawgFlagPool :: FlagPool -- ^ Pool of bits.
  , dawgNumOfStates :: SizeType -- ^ Number of states.
  , dawgNumOfMergedTransitions :: SizeType -- ^ Number of merged transitions.
  , dawgNumOfMergedStates :: SizeType -- ^ Number of merged states.
  , dawgNumOfMergingStates :: SizeType -- ^ Number of merging states.
  }
  deriving (Generic, NFData)


-- | Wrapper around a vector of base units.
newtype BasePool = BasePool { unBasePool :: UV.Vector BaseUnit }
  deriving newtype NFData

-- | Wrapper around a vector of chars.
newtype LabelPool = LabelPool { unLabelPool :: UV.Vector UCharType }
  deriving newtype NFData

-- | Wrapper around 'BitPool'.
newtype FlagPool = FlagPool { unFlagPool :: BitPool }
  deriving newtype NFData

-- | Wrapper around a vector of bits.
newtype BitPool = BitPool { unBitPool :: UV.Vector Bit }
  deriving newtype NFData

-- | Root dawg index. Equivalent to @0@.
root :: BaseType
root = 0
{-# INLINE root #-}

-- | Constructs an empty dawg.
empty :: DAWG
empty = DAWG
  { dawgBasePool = BasePool UV.empty
  , dawgLabelPool = LabelPool UV.empty
  , dawgFlagPool = FlagPool (BitPool UV.empty)
  , dawgNumOfStates = 1
  , dawgNumOfMergedTransitions = 0
  , dawgNumOfMergedStates = 0
  , dawgNumOfMergingStates = 0
  }
{-# INLINE empty #-}

-- | Gets a child by given dawg index.
child :: HasCallStack => BaseType -> DAWG -> BaseType
child !ix = BU.child . (UV.! fromIntegral ix) . unBasePool . dawgBasePool
{-# INLINE child #-}

-- | Gets a sibling by given dawg index.
sibling :: HasCallStack => BaseType -> DAWG -> BaseType
sibling !ix !dawg =
  let hasSibling' = BU.hasSibling . (UV.! fromIntegral ix) . unBasePool . dawgBasePool
      itHasSibling = hasSibling' dawg
  in if itHasSibling then ix + 1 else root
{-# INLINE sibling #-}

-- | Gets a value by given dawg index.
value :: HasCallStack => BaseType -> DAWG -> ValueType
value !ix = BU.value . (UV.! fromIntegral ix) . unBasePool . dawgBasePool
{-# INLINE value #-}

-- | Checks whether the given dawg index is leaf or not.
isLeaf :: HasCallStack => BaseType -> DAWG -> Bool
isLeaf !ix = (== '\0') . chr . fromIntegral . label ix
{-# INLINE isLeaf #-}

-- | Gets a label by given dawg index.
label :: HasCallStack => BaseType -> DAWG -> UCharType
label !ix = (UV.! fromIntegral ix) . unLabelPool . dawgLabelPool
{-# INLINE label #-}

-- | Checks whether given dawg index is merging or not.
isMerging :: HasCallStack => BaseType -> DAWG -> Bool
isMerging !ix = unBit . (UV.! fromIntegral ix) . unBitPool . unFlagPool . dawgFlagPool
{-# INLINE isMerging #-}

-- | Gets a size of a dawg.
size :: DAWG -> SizeType
size = fromIntegral . UV.length . unBasePool . dawgBasePool
{-# INLINE size #-}

-- ** Helpers

-- | Dump dawg state to stdout.
dump :: DAWG -> IO ()
dump DAWG{..} = do
  putStrLn "base_pool"
  let BasePool bpool = dawgBasePool
      LabelPool lpool = dawgLabelPool
      !bs = UV.length bpool
      !ls = UV.length lpool
      !ms = max bs ls

  putStrLn $ concat [ "b(" <> show bs <> ")\tl(" <> show ls <> ")" ]
  
  forM_ [0 .. ms - 1] \ix -> do
    let !b = fromMaybe (BaseUnit 0) (bpool UV.!? ix)
        !l = fromMaybe 0 (lpool UV.!? ix)
    putStrLn $ concat
      [ show b, "\t", show (chr $ fromIntegral l), " (", show l, ")" ]

  putStrLn $ "num_of_states : " <> show dawgNumOfStates
  putStrLn $ "num_of_merged_transitions : " <> show dawgNumOfMergedTransitions
  putStrLn $ "num_of_merged_states : " <> show dawgNumOfMergedStates
  putStrLn $ "num_of_merging_states : " <> show dawgNumOfMergingStates
