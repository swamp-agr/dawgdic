module Data.DAWG.Internal.RankedCompleterNode where

import Data.DAWG.Internal.BaseType

import Data.Char (chr)
import Data.List (intercalate)

data RankedCompleterNode = RankedCompleterNode
  { nodeDictIx :: !BaseType
  , prevNodeIx :: !BaseType
  , nodeLabel :: !UCharType
  , nodeIsQueued :: !Bool
  , nodeHasTerminal :: !Bool
  } deriving (Eq)

instance Show RankedCompleterNode where
  show RankedCompleterNode{..} = intercalate "\t"
    [ show nodeDictIx
    , show prevNodeIx
    , show $ chr $ fromIntegral nodeLabel
    , show nodeIsQueued
    , show nodeHasTerminal
    ]

empty :: RankedCompleterNode
empty = RankedCompleterNode
  { nodeDictIx = 0
  , prevNodeIx = 0
  , nodeLabel = 0
  , nodeIsQueued = False
  , nodeHasTerminal = False
  }
{-# INLINE empty #-}

setDictIx :: BaseType -> RankedCompleterNode -> RankedCompleterNode
setDictIx !dictIx !RankedCompleterNode{..} = RankedCompleterNode { nodeDictIx = dictIx, .. }
{-# INLINE setDictIx #-}

setPrevNodeIx :: BaseType -> RankedCompleterNode -> RankedCompleterNode
setPrevNodeIx !dictIx !RankedCompleterNode{..} =
  RankedCompleterNode { prevNodeIx = dictIx, .. }
{-# INLINE setPrevNodeIx #-}

setLabel :: UCharType -> RankedCompleterNode -> RankedCompleterNode
setLabel !label !RankedCompleterNode{..} = RankedCompleterNode { nodeLabel = label, .. }
{-# INLINE setLabel #-}

setIsQueued :: Bool -> RankedCompleterNode -> RankedCompleterNode
setIsQueued !isQueued !RankedCompleterNode{..} = RankedCompleterNode { nodeIsQueued = isQueued, .. }
{-# INLINE setIsQueued #-}

setHasTerminal :: Bool -> RankedCompleterNode -> RankedCompleterNode
setHasTerminal !hasTerminal !RankedCompleterNode{..} =
  RankedCompleterNode { nodeHasTerminal = hasTerminal, .. }
{-# INLINE setHasTerminal #-}
