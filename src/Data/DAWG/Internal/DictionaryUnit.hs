{-|
Module: Data.DAWG.Internal.DictionaryUnit
Description: Exports dictionary unit as well as its internal API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
{-# LANGUAGE TypeFamilies #-}
module Data.DAWG.Internal.DictionaryUnit where

import Data.Binary
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import Data.Bits
import Data.Vector.Unboxed.Mutable (Unbox)

import Data.DAWG.Internal.BaseType

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Unboxed as UV

-- ** DAWG Dictionary Unit

-- | Unit of a 'Data.DAWG.Internal.Dictionary.Dictionary'
-- or a 'Data.DAWG.Internal.Dictionary.Builder.DictionaryBuilder'.
newtype DictionaryUnit = DictionaryUnit { base :: BaseType }
  deriving newtype (Eq, Ord, Num, Bits, Integral, Real, Enum)

instance Binary DictionaryUnit where
  get = DictionaryUnit <$> getWord32le
  put = putWord32le . base

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

-- | Empty unit. Equivalent to @0@.
empty :: DictionaryUnit
empty = 0
{-# INLINE empty #-}

-- | Maximal offset.
offsetMax :: BaseType
offsetMax = 1 .<<. 21
{-# INLINE offsetMax #-}

-- | @IS_LEAF@ bit.
isLeafBit :: BaseType
isLeafBit = 1 .<<. 31
{-# INLINE isLeafBit #-}

-- | @HAS_LEAF@ bit.
hasLeafBit :: BaseType
hasLeafBit = 1 .<<. 8
{-# INLINE hasLeafBit #-}

-- | @EXTENSION@ bit.
extensionBit :: BaseType
extensionBit = 1 .<<. 9
{-# INLINE extensionBit #-}

-- | Sets @HAS_LEAF@ bit to 'DictionaryUnit'.
setHasLeaf :: DictionaryUnit -> DictionaryUnit
setHasLeaf u = u .|. fromIntegral hasLeafBit
{-# INLINE setHasLeaf #-}

-- | Sets a value as @IS_LEAF@ bit to 'DictionaryUnit'.
setValue :: ValueType -> DictionaryUnit -> DictionaryUnit
setValue v u
  = u { base = fromIntegral v .|. isLeafBit }
{-# INLINE setValue #-}

-- | Sets a label to 'DictionaryUnit'.
setLabel :: UCharType -> DictionaryUnit -> DictionaryUnit
setLabel l u
  = (u .&. complement (0xFF :: DictionaryUnit)) .|. fromIntegral l
{-# INLINE setLabel #-}

-- | Sets an offset to a non-leaf unit. Returns flag as result of setting offset.
--
-- * 'True' if offset has been set to a non-leaf unit.
-- * 'False' if offset was not set.
--
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

-- | Checks whether @HAS_LEAF@ bit is set or not.
hasLeaf :: DictionaryUnit -> Bool
hasLeaf u = (base u .&. hasLeafBit) /= 0
{-# INLINE hasLeaf #-}

-- | Gets a value of 'DictionaryUnit'.
value :: DictionaryUnit -> ValueType
value (DictionaryUnit u)
  = fromIntegral (u .&. (complement isLeafBit))
{-# INLINE value #-}

-- | Gets a label of 'DictionaryUnit'.
label :: DictionaryUnit -> BaseType
label u = fromIntegral u .&. (isLeafBit .|. 0xFF)
{-# INLINE label #-}

-- | Calculates offset of 'DictionaryUnit'.
offset :: DictionaryUnit -> BaseType
offset (DictionaryUnit b)
  = (b .>>. 10) .<<. fromIntegral ((b .&. extensionBit) .>>. 6)
{-# INLINE offset #-}
