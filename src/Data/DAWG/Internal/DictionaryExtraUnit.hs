{-# LANGUAGE TypeFamilies #-}
module Data.DAWG.Internal.DictionaryExtraUnit where

import Data.Bits
import Data.Coerce
import Data.Vector.Unboxed.Mutable (Unbox)
import GHC.Generics (Generic)

import Data.DAWG.Internal.BaseType

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Unboxed as UV

-- ** DAWG Dictionary Extra Unit

newtype DictionaryExtraUnit = DictionaryExtraUnit
  { unExtraUnit
    :: ( BaseType -- lo values
       , BaseType -- hi values
       )
  }
  deriving (Generic, Eq)

instance Show DictionaryExtraUnit where
  show e@(DictionaryExtraUnit (lo, hi)) = concat
    [ "((", show lo, ",", show $ isFixed e, ",", show $ next e
    , "),(", show hi, ",", show $ isUsed e, ",", show $ prev e, "))"]
  {-# INLINE show #-}

newtype instance UV.MVector s DictionaryExtraUnit =
  MV_DictionaryExtraUnit (UV.MVector s (BaseType, BaseType))
newtype instance UV.Vector DictionaryExtraUnit =
  V_DictionaryExtraUnit (UV.Vector (BaseType, BaseType))

deriving newtype instance V.MVector UV.MVector DictionaryExtraUnit
deriving newtype instance VG.Vector UV.Vector DictionaryExtraUnit
deriving newtype instance Unbox DictionaryExtraUnit

empty :: DictionaryExtraUnit
empty = DictionaryExtraUnit (0, 0)
{-# INLINE empty #-}

setIsFixed :: DictionaryExtraUnit -> DictionaryExtraUnit
setIsFixed (DictionaryExtraUnit (!lo, !hi)) =
  DictionaryExtraUnit (lo .|. 1, hi)
{-# INLINE setIsFixed #-}

setNext
  :: BaseType -> DictionaryExtraUnit -> DictionaryExtraUnit
setNext !next' (DictionaryExtraUnit (!lo, !hi)) =
  DictionaryExtraUnit ((lo .&. 1) .|. (next' .<<. 1), hi)
{-# INLINE setNext #-}

setIsUsed :: DictionaryExtraUnit -> DictionaryExtraUnit
setIsUsed (DictionaryExtraUnit (!lo, !hi)) =
  DictionaryExtraUnit (lo, hi .|. 1)
{-# INLINE setIsUsed #-}

setPrev
  :: BaseType -> DictionaryExtraUnit -> DictionaryExtraUnit
setPrev !prev' (DictionaryExtraUnit (!lo, !hi)) =
  DictionaryExtraUnit (lo, (hi .&. 1) .|. (prev' .<<. 1))
{-# INLINE setPrev #-}

isFixed :: DictionaryExtraUnit -> Bool
isFixed = (== 1) . (.&. 1) . fst . coerce @_ @(BaseType, BaseType)
{-# INLINE isFixed #-}

next :: DictionaryExtraUnit -> BaseType
next = (.>>. 1) . fst . coerce @_ @(BaseType, BaseType)
{-# INLINE next #-}

isUsed :: DictionaryExtraUnit -> Bool
isUsed = (== 1) . (.&. 1) . snd . coerce @_ @(BaseType, BaseType)
{-# INLINE isUsed #-}

prev :: DictionaryExtraUnit -> BaseType
prev = (.>>. 1) . snd . coerce @_ @(BaseType, BaseType)
{-# INLINE prev #-}
