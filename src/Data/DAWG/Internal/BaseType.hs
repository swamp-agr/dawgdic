{-|
Module: Data.DAWG.Internal.BaseType
Description: Exports base types used across the library.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
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

-- | 32-bit mix function. <http://web.archive.org/19991104155419/www.concentric.net/~Ttwang/tech/inthash.htm>
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

-- | 32 or 64-bit unsigned integer.
type SizeType = Word

-- | Alias for unboxed mutable vector.
type ObjectPool = UVM.MVector

-- | Alias for vector hashtable with unboxed keys and unboxed values.
type UUHT m k v = HT.Dictionary (PrimState m) UV.MVector k UV.MVector v

-- | Alias for vector hashtable with unboxed keys and unboxed nested 'UUHT' (with given keys and values) as values.
type UHHT m k v = HT.Dictionary (PrimState m) UV.MVector k UM.MVector (UUHT m k v)

