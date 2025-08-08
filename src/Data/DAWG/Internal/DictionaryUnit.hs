{-# LANGUAGE TypeFamilies #-}
module Data.DAWG.Internal.DictionaryUnit where

import Data.Binary
import Data.Bits
import Data.Vector.Unboxed.Mutable (Unbox)

import Data.DAWG.Internal.BaseType

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Unboxed as UV

-- ** DAWG Dictionary Unit

newtype DictionaryUnit = DictionaryUnit { base :: BaseType }
  deriving newtype (Eq, Ord, Num, Bits, Integral, Real, Enum, Binary)

instance Show DictionaryUnit where
  show u = concat
    [ "(", show $ base u, ",", show $ hasLeaf u, ",", show $ value u, ",", show $ label u, ",", show $ offset u, ")"]
  {-# INLINE show #-}

newtype instance UV.MVector s DictionaryUnit =
  MV_DictionaryUnit (UV.MVector s BaseType)
newtype instance UV.Vector DictionaryUnit = V_DictionaryUnit (UV.Vector BaseType)

deriving newtype instance V.MVector UV.MVector DictionaryUnit
deriving newtype instance VG.Vector UV.Vector DictionaryUnit
deriving newtype instance Unbox DictionaryUnit

size :: SizeType
size = baseTypeSize
{-# INLINE size #-}

empty :: DictionaryUnit
empty = 0
{-# INLINE empty #-}

offsetMax :: BaseType
offsetMax = 1 .<<. 21
{-# INLINE offsetMax #-}

isLeafBit :: BaseType
isLeafBit = 1 .<<. 31
{-# INLINE isLeafBit #-}

hasLeafBit :: BaseType
hasLeafBit = 1 .<<. 8
{-# INLINE hasLeafBit #-}

extensionBit :: BaseType
extensionBit = 1 .<<. 9
{-# INLINE extensionBit #-}

setHasLeaf :: DictionaryUnit -> DictionaryUnit
setHasLeaf u = u .|. fromIntegral hasLeafBit
{-# INLINE setHasLeaf #-}

setValue :: ValueType -> DictionaryUnit -> DictionaryUnit
setValue v u
  = u { base = fromIntegral v .|. isLeafBit }
{-# INLINE setValue #-}

setLabel :: UCharType -> DictionaryUnit -> DictionaryUnit
setLabel l u
  = (u .&. complement (0xFF :: DictionaryUnit)) .|. fromIntegral l
{-# INLINE setLabel #-}

-- | Sets an offset to a non-leaf unit.
setOffset :: BaseType -> DictionaryUnit -> (Bool, DictionaryUnit)
setOffset offset' (DictionaryUnit b)
  | offset' >= (offsetMax .<<. 8) = (False, DictionaryUnit b)
  | otherwise =
    let base0 = b .&. (isLeafBit .|. hasLeafBit .|. 0xFF)
        base1 = if offset' < offsetMax
          then base0 .|. (offset' .<<. 10)
          else base0 .|. (offset' .<<. 2) .|. extensionBit
    in (True, DictionaryUnit base1)
{-# INLINE setOffset #-}

hasLeaf :: DictionaryUnit -> Bool
hasLeaf u = (base u .&. hasLeafBit) /= 0
{-# INLINE hasLeaf #-}

value :: DictionaryUnit -> ValueType
value (DictionaryUnit u)
  = fromIntegral (u .&. (complement isLeafBit))
{-# INLINE value #-}

label :: DictionaryUnit -> BaseType
label u = fromIntegral u .&. (isLeafBit .|. 0xFF)
{-# INLINE label #-}

offset :: DictionaryUnit -> BaseType
offset (DictionaryUnit b)
  = (b .>>. 10) .<<. fromIntegral ((b .&. extensionBit) .>>. 6)
{-# INLINE offset #-}
