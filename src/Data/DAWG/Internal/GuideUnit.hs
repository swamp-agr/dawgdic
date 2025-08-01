{-# LANGUAGE TypeFamilies #-}
module Data.DAWG.Internal.GuideUnit where

import Data.Binary
import Data.Vector.Unboxed.Mutable (Unbox)

import Data.DAWG.Internal.BaseType

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable  as V
import qualified Data.Vector.Unboxed as UV 

-- ** Guide Unit

newtype GuideUnit = GuideUnit
  { unGuideUnit
    :: ( UCharType -- child
       , UCharType -- sibling
       )
  }
  deriving newtype (Show, Binary)

newtype instance UV.MVector s GuideUnit = MV_GuideUnit (UV.MVector s (UCharType, UCharType))
newtype instance UV.Vector GuideUnit = V_GuideUnit (UV.Vector (UCharType, UCharType))

deriving newtype instance V.MVector UV.MVector GuideUnit
deriving newtype instance VG.Vector UV.Vector GuideUnit
deriving newtype instance Unbox GuideUnit

empty :: GuideUnit
empty = GuideUnit (0, 0)

child :: GuideUnit -> UCharType
child (GuideUnit (!child', !_)) = child'

sibling :: GuideUnit -> UCharType
sibling (GuideUnit (!_, !sibling')) = sibling'

setChild :: UCharType -> GuideUnit -> GuideUnit
setChild !newChild (GuideUnit (!_oldChild, !sibling')) =
  GuideUnit (newChild, sibling')

setSibling :: UCharType -> GuideUnit -> GuideUnit
setSibling !newSibling (GuideUnit (!child', !_oldSibling)) =
  GuideUnit (child', newSibling)
