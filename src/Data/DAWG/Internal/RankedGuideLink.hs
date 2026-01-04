{-# LANGUAGE TypeFamilies #-}
module Data.DAWG.Internal.RankedGuideLink where

import Data.Vector.Unboxed.Mutable (Unbox)

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable  as V
import qualified Data.Vector.Unboxed as UV 

import Data.DAWG.Internal.BaseType

import Data.Char (chr)

newtype RankedGuideLink = RankedGuideLink
  { rankedGuideLink
    :: ( UCharType -- label
       , ValueType -- value
       )
  }
  deriving newtype Eq

-- For sorting links in descending value order
instance Ord RankedGuideLink where
  lhs `compare` rhs = if value lhs /= value rhs
    then value rhs `compare` value lhs
    else label lhs `compare` label rhs

instance Show RankedGuideLink where
  show (RankedGuideLink (l, v)) = concat ["'", [chr $ fromIntegral l], "'\t", show v]

newtype instance UV.MVector s RankedGuideLink
  = MV_RankedGuideLink (UV.MVector s (UCharType, ValueType))
newtype instance UV.Vector RankedGuideLink
  = V_RankedGuideLink (UV.Vector (UCharType, ValueType))

deriving newtype instance V.MVector UV.MVector RankedGuideLink
deriving newtype instance VG.Vector UV.Vector RankedGuideLink
deriving newtype instance Unbox RankedGuideLink

setLabel :: UCharType -> RankedGuideLink -> RankedGuideLink
setLabel !l (RankedGuideLink (!_, !v)) = RankedGuideLink (l, v)

setValue :: ValueType -> RankedGuideLink -> RankedGuideLink
setValue !v (RankedGuideLink (!l, !_)) = RankedGuideLink (l, v)

label :: RankedGuideLink -> UCharType
label = fst . rankedGuideLink

value :: RankedGuideLink -> ValueType
value = snd . rankedGuideLink
