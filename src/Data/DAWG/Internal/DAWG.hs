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

data DAWG = DAWG
  { dawgBasePool :: BasePool
  , dawgLabelPool :: LabelPool
  , dawgFlagPool :: FlagPool
  , dawgNumOfStates :: SizeType
  , dawgNumOfMergedTransitions :: SizeType
  , dawgNumOfMergedStates :: SizeType
  , dawgNumOfMergingStates :: SizeType
  }
  deriving (Generic, NFData)


newtype BasePool = BasePool { unBasePool :: UV.Vector BaseUnit }
  deriving newtype NFData

newtype LabelPool = LabelPool { unLabelPool :: UV.Vector UCharType }
  deriving newtype NFData

newtype FlagPool = FlagPool { unFlagPool :: BitPool }
  deriving newtype NFData

newtype BitPool = BitPool { unBitPool :: UV.Vector Bit }
  deriving newtype NFData

root :: BaseType
root = 0
{-# INLINE root #-}

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

child :: HasCallStack => BaseType -> DAWG -> BaseType
child !ix = BU.child . (UV.! fromIntegral ix) . unBasePool . dawgBasePool
{-# INLINE child #-}

sibling :: HasCallStack => BaseType -> DAWG -> BaseType
sibling !ix !dawg =
  let hasSibling' = BU.hasSibling . (UV.! fromIntegral ix) . unBasePool . dawgBasePool
      itHasSibling = hasSibling' dawg
  in if itHasSibling then ix + 1 else root
{-# INLINE sibling #-}

value :: HasCallStack => BaseType -> DAWG -> ValueType
value !ix = BU.value . (UV.! fromIntegral ix) . unBasePool . dawgBasePool
{-# INLINE value #-}

isLeaf :: HasCallStack => BaseType -> DAWG -> Bool
isLeaf !ix = (== '\0') . chr . fromIntegral . label ix
{-# INLINE isLeaf #-}

label :: HasCallStack => BaseType -> DAWG -> UCharType
label !ix = (UV.! fromIntegral ix) . unLabelPool . dawgLabelPool
{-# INLINE label #-}

isMerging :: HasCallStack => BaseType -> DAWG -> Bool
isMerging !ix = unBit . (UV.! fromIntegral ix) . unBitPool . unFlagPool . dawgFlagPool
{-# INLINE isMerging #-}

size :: DAWG -> SizeType
size = fromIntegral . UV.length . unBasePool . dawgBasePool
{-# INLINE size #-}

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
