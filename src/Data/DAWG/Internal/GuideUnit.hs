{-|
Module: Data.DAWG.Internal.GuideUnit
Description: Exports guide unit as well as its internal API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
{-# LANGUAGE TypeFamilies #-}
module Data.DAWG.Internal.GuideUnit where

import Control.DeepSeq (NFData)
import Data.Binary
import Data.Vector.Unboxed.Mutable (Unbox)

import Data.DAWG.Internal.BaseType

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable  as V
import qualified Data.Vector.Unboxed as UV 

-- ** Guide Unit

-- | Unit of a 'Data.DAWG.Internal.Guide' or a 'Data.DAWG.Internal.GuideBuilder'.
-- Contains an information about child character and sibling character.
--
-- * Input characters should be mapped in the 0-255 range.
--
newtype GuideUnit = GuideUnit
  { unGuideUnit
    :: ( UCharType -- child
       , UCharType -- sibling
       )
  }
  deriving newtype (Show, Binary, NFData)

newtype instance UV.MVector s GuideUnit = MV_GuideUnit (UV.MVector s (UCharType, UCharType))
newtype instance UV.Vector GuideUnit = V_GuideUnit (UV.Vector (UCharType, UCharType))

deriving newtype instance V.MVector UV.MVector GuideUnit
deriving newtype instance VG.Vector UV.Vector GuideUnit
deriving newtype instance Unbox GuideUnit

-- | Empty unit. Equivalent to @0@.
empty :: GuideUnit
empty = GuideUnit (0, 0)
{-# INLINE empty #-}

-- | Gets the child character.
child :: GuideUnit -> UCharType
child (GuideUnit (!child', !_)) = child'
{-# INLINE child #-}

-- | Gets the sibling character.
sibling :: GuideUnit -> UCharType
sibling (GuideUnit (!_, !sibling')) = sibling'
{-# INLINE sibling #-}

-- | Sets a child character to the given unit.
setChild :: UCharType -> GuideUnit -> GuideUnit
setChild !newChild (GuideUnit (!_oldChild, !sibling')) =
  GuideUnit (newChild, sibling')
{-# INLINE setChild #-}

-- | Sets a sibling character to the given unit.
setSibling :: UCharType -> GuideUnit -> GuideUnit
setSibling !newSibling (GuideUnit (!child', !_oldSibling)) =
  GuideUnit (child', newSibling)
{-# INLINE setSibling #-}
