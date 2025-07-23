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

newtype BaseUnit = BaseUnit { unBaseUnit :: BaseType }
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
  hash (BaseUnit !u) =
    let !k0 = (complement u) + (u .<<. 15)
        !k1 = k0 .^. (k0 .>>. 12)
        !k2 = k1 + (k1 .<<. 2)
        !k3 = k2 .^. (k2 .>>. 4)
        !k4 = k3 * 2057 -- k4 = k3 + (k3 .<<. 3) + (k3 .<<. 11)
        !k5 = k4 .^. (k4 .>>. 16)
    in fromIntegral k5
  hashWithSalt s u = hash s .^. hash u

empty :: BaseUnit
empty = BaseUnit 0

base :: BaseUnit -> BaseType
base = unBaseUnit

child :: BaseUnit -> BaseType
child u = base u .>>. 2

hasSibling :: BaseUnit -> Bool
hasSibling u = (base u .&. 1) /= 0

value :: BaseUnit -> ValueType
value u = fromIntegral $! base u .>>. 1

isState :: BaseUnit -> Bool
isState !u = (base u .&. 2) /= 0
