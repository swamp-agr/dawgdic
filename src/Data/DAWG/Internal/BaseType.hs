module Data.DAWG.Internal.BaseType where

import Control.Monad.Primitive (PrimState)
import Data.Bits
import Data.Int
import Data.Word

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM
import qualified Data.Vector.Mutable as UM
import qualified Data.Vector.Hashtables as HT

-- ** Base Types

-- | unsigned char (8-bit).
type UCharType = Word8

-- | char (8-bit).
type CharType = Int8

-- | 32-bit unsigned integer.
type BaseType = Word32

baseTypeSize :: SizeType
baseTypeSize = 4

hashBaseType :: BaseType -> Int
hashBaseType u =
    let !k0 = (complement u) + (u .<<. 15)
        !k1 = k0 .^. (k0 .>>. 12)
        !k2 = k1 + (k1 .<<. 2)
        !k3 = k2 .^. (k2 .>>. 4)
        !k4 = k3 * 2057 -- k4 = k3 + (k3 .<<. 3) + (k3 .<<. 11)
        !k5 = k4 .^. (k4 .>>. 16)
    in fromIntegral k5
{-# INLINE hashBaseType #-}

-- | 32-bit integer.
type ValueType = Int32

-- | 32 or 64-bit unsigned integer
type SizeType = Word

type ObjectPool = UVM.MVector

type UUHT m k v = HT.Dictionary (PrimState m) UV.MVector k UV.MVector v

type UHHT m k v = HT.Dictionary (PrimState m) UV.MVector k UM.MVector (UUHT m k v)

