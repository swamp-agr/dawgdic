{-|
Module: Data.DAWG.Internal.DAWGUnit
Description: Exports dawg unit used in dawg and dawg builder as well as its internal API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
{-# LANGUAGE TypeFamilies #-}
module Data.DAWG.Internal.DAWGUnit where

import Data.Binary
import Data.Bits
import Data.Char
import Data.Vector.Unboxed.Mutable (Unbox)

import Data.DAWG.Internal.BaseType

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable  as V
import qualified Data.Vector.Unboxed as UV

-- ** DAWG Unit

-- | Unit of a 'Data.DAWG.Internal.DAWGBuilder.DAWGBuilder'. Contains following information:
--
-- * child (4 bytes);
-- * sibling (4 bytes);
-- * label (0-255);
-- * is state flag (0-1);
-- * has sibling flag (0-1).
--
-- Constructed as unboxed tuple of all its properties.
newtype DAWGUnit = DAWGUnit
  { unDawgUnit
      :: ( BaseType -- child
         , BaseType -- sibling
         , UCharType -- label
         , Bool -- isState
         , Bool -- hasSibling
         )
  }
  deriving newtype (Binary)

instance Show DAWGUnit where
  show u@(DAWGUnit (c, s, l, is, hs)) =
    concat
      [ "(", show c, ",", show s, ",'"
      , pure (chr $ fromIntegral l), "' (", show l, "),"
      , showBool is, ",", showBool hs, ") b ", show (base u)
      ]
    where
      showBool x = if x then "1" else "0"
      {-# INLINE showBool #-}
  {-# INLINE show #-}

newtype instance UV.MVector s DAWGUnit = MV_DAWGUnit (UV.MVector s (BaseType, BaseType, UCharType, Bool, Bool))
newtype instance UV.Vector DAWGUnit = V_DAWGUnit (UV.Vector (BaseType, BaseType, UCharType, Bool, Bool))

deriving newtype instance V.MVector UV.MVector DAWGUnit
deriving newtype instance VG.Vector UV.Vector DAWGUnit
deriving newtype instance Unbox DAWGUnit

-- | Empty unit. Equivalent to @0@.
empty :: DAWGUnit
empty = DAWGUnit (0, 0, 0, False, False)
{-# INLINE empty #-}

-- | Gets a value of a unit. Synonym for 'child'.
value :: DAWGUnit -> ValueType
value = fromIntegral . child
{-# INLINE value #-}

-- | Calculates a base value of a unit.
base :: DAWGUnit -> BaseType
base u =
  if label u == fromIntegral (ord '\0')
    then (child u .<<. 1) .|. (if hasSibling u then 1 else 0)
    else (child u .<<. 2)
       .|. (if isState u then 2 else 0)
       .|. (if hasSibling u then 1 else 0)
{-# INLINE base #-}

-- | Gets a child from the unit.
child :: DAWGUnit -> BaseType
child (DAWGUnit (!c, !_, !_, !_, !_)) = c
{-# INLINE child #-}

-- | Gets a sibling from the unit.
sibling :: DAWGUnit -> BaseType
sibling (DAWGUnit (!_, !s, !_, !_, !_)) = s
{-# INLINE sibling #-}

-- | Gets a label from the unit.
label :: DAWGUnit -> UCharType
label (DAWGUnit (!_, !_, !l, !_, !_)) = l
{-# INLINE label #-}

-- | Checks whether a unit is a state or not.
isState :: DAWGUnit -> Bool
isState (DAWGUnit (!_, !_, !_, !is, !_)) = is
{-# INLINE isState #-}

-- | Checks whether a unit has a sibling or not.
hasSibling :: DAWGUnit -> Bool
hasSibling (DAWGUnit (!_, !_, !_, !_, !hs)) = hs
{-# INLINE hasSibling #-}

-- | Sets a child to the unit.
setChild :: DAWGUnit -> BaseType -> DAWGUnit
setChild (DAWGUnit (!_, !s, !l, !is, !hs)) !c = DAWGUnit (c, s, l, is, hs)
{-# INLINE setChild #-}

-- | Sets a sibling to the unit.
setSibling :: DAWGUnit -> BaseType -> DAWGUnit
setSibling (DAWGUnit (!c, !_, !l, !is, !hs)) !s = DAWGUnit (c, s, l, is, hs)
{-# INLINE setSibling #-}

-- | Sets a label to the unit.
setLabel :: DAWGUnit -> UCharType -> DAWGUnit
setLabel (DAWGUnit (!c, !s, !_, !is, !hs)) !l = DAWGUnit (c, s, l, is, hs)
{-# INLINE setLabel #-}

-- | Sets a flag @isState@ to the unit.
setIsState :: DAWGUnit -> Bool -> DAWGUnit
setIsState (DAWGUnit (!c, !s, !l, !_, !hs)) !is = DAWGUnit (c, s, l, is, hs)
{-# INLINE setIsState #-}

-- | Sets a flag @hasSibling@ to the unit.
setHasSibling :: DAWGUnit -> Bool -> DAWGUnit
setHasSibling (DAWGUnit (!c, !s, !l, !is, !_)) !hs = DAWGUnit (c, s, l, is, hs)
{-# INLINE setHasSibling #-}
