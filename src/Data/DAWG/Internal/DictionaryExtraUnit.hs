{-|
Module: Data.DAWG.Internal.DictionaryExtraUnit
Description: Exports dictionary extra unit as well as its internal API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
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

-- | Extra unit for supportive circular linked list.
-- Used in 'Data.DAWG.Internal.DictionaryBuilder.DictionaryBuilder'.
-- Contains a following information:
--
-- * low values;
-- * high values.
--
-- Constructed as unboxed tuple of two bases.
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

-- | Empty unit. Equivalent to @0@.
empty :: DictionaryExtraUnit
empty = DictionaryExtraUnit (0, 0)
{-# INLINE empty #-}

-- | Sets @isFixed@ flag to 'DictionaryExtraUnit'.
setIsFixed :: DictionaryExtraUnit -> DictionaryExtraUnit
setIsFixed (DictionaryExtraUnit (!lo, !hi)) =
  DictionaryExtraUnit (lo .|. 1, hi)
{-# INLINE setIsFixed #-}

-- | Sets @next@ value (link to the next unit) to 'DictionaryExtraUnit'.
setNext
  :: BaseType -> DictionaryExtraUnit -> DictionaryExtraUnit
setNext !next' (DictionaryExtraUnit (!lo, !hi)) =
  DictionaryExtraUnit ((lo .&. 1) .|. (next' .<<. 1), hi)
{-# INLINE setNext #-}

-- | Sets @isUsed@ flag to 'DictionaryExtraUnit'.
setIsUsed :: DictionaryExtraUnit -> DictionaryExtraUnit
setIsUsed (DictionaryExtraUnit (!lo, !hi)) =
  DictionaryExtraUnit (lo, hi .|. 1)
{-# INLINE setIsUsed #-}

-- | Set @prev@ value  (link to the previous unit) to 'DictionaryExtraUnit'.
setPrev
  :: BaseType -> DictionaryExtraUnit -> DictionaryExtraUnit
setPrev !prev' (DictionaryExtraUnit (!lo, !hi)) =
  DictionaryExtraUnit (lo, (hi .&. 1) .|. (prev' .<<. 1))
{-# INLINE setPrev #-}

-- | Checks whether the unit is fixed or not.
isFixed :: DictionaryExtraUnit -> Bool
isFixed = (== 1) . (.&. 1) . fst . coerce @_ @(BaseType, BaseType)
{-# INLINE isFixed #-}

-- | Gets the next unit.
next :: DictionaryExtraUnit -> BaseType
next = (.>>. 1) . fst . coerce @_ @(BaseType, BaseType)
{-# INLINE next #-}

-- | Checks whether the unit is used or not.
isUsed :: DictionaryExtraUnit -> Bool
isUsed = (== 1) . (.&. 1) . snd . coerce @_ @(BaseType, BaseType)
{-# INLINE isUsed #-}

-- | Gets the previous unit. 
prev :: DictionaryExtraUnit -> BaseType
prev = (.>>. 1) . snd . coerce @_ @(BaseType, BaseType)
{-# INLINE prev #-}
