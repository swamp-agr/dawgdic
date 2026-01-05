{-|
Module: Data.DAWG.Internal.RankedCompleterCandidate
Description: Exports ranked completer node candidate with associated value.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.Internal.RankedCompleterCandidate where

import Data.DAWG.Internal.BaseType

-- ** Ranked completer candidate

-- | Candidate to a completion result ranked by value in descending order.
data RankedCompleterCandidate = RankedCompleterCandidate
  { candidateNodeIx :: !BaseType -- ^ Index of a node of a candidate for completion.
  , candidateValue :: !ValueType -- ^ Value associated with a candidate for completion.
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

-- | Empty candidate which holds root node index and negative value.
empty :: RankedCompleterCandidate
empty = RankedCompleterCandidate
  { candidateNodeIx = 0
  , candidateValue = -1
  }
{-# INLINE empty #-}

-- | Sets a node index to the given candidate.
setNodeIx :: BaseType -> RankedCompleterCandidate -> RankedCompleterCandidate
setNodeIx !nodeIx !RankedCompleterCandidate{..} =
  RankedCompleterCandidate { candidateNodeIx = nodeIx, .. }
{-# INLINE setNodeIx #-}

-- | Sets a value to the given candidate.
setValue :: ValueType -> RankedCompleterCandidate -> RankedCompleterCandidate
setValue !value !RankedCompleterCandidate{..} =
  RankedCompleterCandidate { candidateValue = value, .. }
{-# INLINE setValue #-}
