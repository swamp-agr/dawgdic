{-|
Module: Data.DAWG.Internal.BaseUnit
Description: Exports base unit used in dawg.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
{-# LANGUAGE TypeFamilies #-}
module Data.DAWG.Internal.BaseUnit where

import Data.Binary
import Data.Bits
import Data.Hashable (Hashable (..))
import Data.Primitive.Types (Prim)
import Data.Vector.Unboxed.Mutable (Unbox)

import Data.DAWG.Internal.BaseType

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Unboxed as UV

-- ** Base Unit

-- | Base unit. Used as a base for DAWG and DAWG builder.
newtype BaseUnit = BaseUnit { base :: BaseType }
  deriving newtype (Eq, Prim, Read, Binary)

instance Show BaseUnit where
  show !b = concat
    [ "(", show $ base b
    , ",", show $ child b
    , ",", show $ hasSibling b
    , ",", show $ isState b
    , ",", show $ value b
    , ")"
    ]

newtype instance UV.MVector s BaseUnit = MV_BaseUnit (UV.MVector s BaseType)
newtype instance UV.Vector BaseUnit = V_BaseUnit (UV.Vector BaseType)

deriving newtype instance V.MVector UV.MVector BaseUnit
deriving newtype instance VG.Vector UV.Vector BaseUnit
deriving newtype instance Unbox BaseUnit

instance Hashable BaseUnit where
  -- 32-bit mix function
  -- http://www.concentric.net/~Ttwang/tech/inthash.htm
  hash (BaseUnit !u) = hashBaseType u
  {-# INLINE hash #-}

  hashWithSalt s u = hash s .^. hash u
  {-# INLINE hashWithSalt #-}

-- | Empty base unit. Equivalent to @0@.
empty :: BaseUnit
empty = BaseUnit 0
{-# INLINE empty #-}

-- | Gets a child unit or @0@.
child :: BaseUnit -> BaseType
child u = base u .>>. 2
{-# INLINE child #-}

-- | Checks whether base unit has a sibling or not.
hasSibling :: BaseUnit -> Bool
hasSibling u = (base u .&. 1) /= 0
{-# INLINE hasSibling #-}

-- | Gets a value associated with a base unit or @0@.
value :: BaseUnit -> ValueType
value u = fromIntegral $! base u .>>. 1
{-# INLINE value #-}

-- | Checks whether base unit is a state or not.
isState :: BaseUnit -> Bool
isState !u = (base u .&. 2) /= 0
{-# INLINE isState #-}
