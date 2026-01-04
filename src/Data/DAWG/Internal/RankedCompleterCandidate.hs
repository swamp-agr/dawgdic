module Data.DAWG.Internal.RankedCompleterCandidate where

import Data.DAWG.Internal.BaseType

data RankedCompleterCandidate = RankedCompleterCandidate
  { candidateNodeIx :: !BaseType
  , candidateValue :: !ValueType
  } deriving (Eq)

instance Ord RankedCompleterCandidate where
  lhs `compare` rhs = if candidateValue lhs /= candidateValue rhs
    then candidateValue rhs `compare` candidateValue lhs
    else candidateNodeIx lhs `compare` candidateNodeIx rhs

instance Show RankedCompleterCandidate where
  show RankedCompleterCandidate{..} = concat [ show candidateNodeIx, "\t", show candidateValue]

instance Monoid RankedCompleterCandidate where
  mempty = empty

instance Semigroup RankedCompleterCandidate where
  _a <> b = b

empty :: RankedCompleterCandidate
empty = RankedCompleterCandidate
  { candidateNodeIx = 0
  , candidateValue = -1
  }
{-# INLINE empty #-}

setNodeIx :: BaseType -> RankedCompleterCandidate -> RankedCompleterCandidate
setNodeIx !nodeIx !RankedCompleterCandidate{..} =
  RankedCompleterCandidate { candidateNodeIx = nodeIx, .. }
{-# INLINE setNodeIx #-}

setValue :: ValueType -> RankedCompleterCandidate -> RankedCompleterCandidate
setValue !value !RankedCompleterCandidate{..} =
  RankedCompleterCandidate { candidateValue = value, .. }
{-# INLINE setValue #-}
