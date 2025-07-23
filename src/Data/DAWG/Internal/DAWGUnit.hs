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

-- | Unit for building a dawg.
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

newtype instance UV.MVector s DAWGUnit = MV_DAWGUnit (UV.MVector s (BaseType, BaseType, UCharType, Bool, Bool))
newtype instance UV.Vector DAWGUnit = V_DAWGUnit (UV.Vector (BaseType, BaseType, UCharType, Bool, Bool))

deriving newtype instance V.MVector UV.MVector DAWGUnit
deriving newtype instance VG.Vector UV.Vector DAWGUnit
deriving newtype instance Unbox DAWGUnit

empty :: DAWGUnit
empty = DAWGUnit (0, 0, 0, False, False)

value :: DAWGUnit -> ValueType
value = fromIntegral . child

-- | Calculates a base value of a unit.
base :: DAWGUnit -> BaseType
base u =
  if label u == fromIntegral (ord '\0')
    then (child u .<<. 1) .|. (if hasSibling u then 1 else 0)
    else (child u .<<. 2)
       .|. (if isState u then 2 else 0)
       .|. (if hasSibling u then 1 else 0)

child :: DAWGUnit -> BaseType
child (DAWGUnit (!c, !_, !_, !_, !_)) = c

sibling :: DAWGUnit -> BaseType
sibling (DAWGUnit (!_, !s, !_, !_, !_)) = s

label :: DAWGUnit -> UCharType
label (DAWGUnit (!_, !_, !l, !_, !_)) = l

isState :: DAWGUnit -> Bool
isState (DAWGUnit (!_, !_, !_, !is, !_)) = is

hasSibling :: DAWGUnit -> Bool
hasSibling (DAWGUnit (!_, !_, !_, !_, !hs)) = hs

setChild :: DAWGUnit -> BaseType -> DAWGUnit
setChild (DAWGUnit (!_, !s, !l, !is, !hs)) !c = DAWGUnit (c, s, l, is, hs)

setSibling :: DAWGUnit -> BaseType -> DAWGUnit
setSibling (DAWGUnit (!c, !_, !l, !is, !hs)) !s = DAWGUnit (c, s, l, is, hs)

setLabel :: DAWGUnit -> UCharType -> DAWGUnit
setLabel (DAWGUnit (!c, !s, !_, !is, !hs)) !l = DAWGUnit (c, s, l, is, hs)

setIsState :: DAWGUnit -> Bool -> DAWGUnit
setIsState (DAWGUnit (!c, !s, !l, !_, !hs)) !is = DAWGUnit (c, s, l, is, hs)

setHasSibling :: DAWGUnit -> Bool -> DAWGUnit
setHasSibling (DAWGUnit (!c, !s, !l, !is, !_)) !hs = DAWGUnit (c, s, l, is, hs)
