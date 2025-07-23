module Data.DAWG.Internal.BaseType where

import Control.Monad.Primitive (PrimState)
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

-- | 32-bit integer.
type ValueType = Int32

-- | 32 or 64-bit unsigned integer
type SizeType = Word

type ObjectPool = UVM.MVector

type UUHT m k v = HT.Dictionary (PrimState m) UV.MVector k UV.MVector v

type UHHT m k v = HT.Dictionary (PrimState m) UV.MVector k UM.MVector (UUHT m k v)

trace str = HT.unsafeIOToPrim . putStrLn $ str
