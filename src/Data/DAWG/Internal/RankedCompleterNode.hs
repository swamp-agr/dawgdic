{-|
Module: Data.DAWG.Internal.RankedCompleterNode
Description: Exports ranked completer node with its API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.Internal.RankedCompleterNode where

import Data.DAWG.Internal.BaseType

import Data.Char (chr)
import Data.List (intercalate)

-- ** Ranked completer node

-- | Ranked completer node.
data RankedCompleterNode = RankedCompleterNode
  { nodeDictIx :: !BaseType -- ^ Dictionary index of a node.
  , prevNodeIx :: !BaseType -- ^ Index of a previous completer node.
  , nodeLabel :: !UCharType -- ^ Character associated with a node.
  , nodeIsQueued :: !Bool -- ^ 'True', if node is being queued.
  , nodeHasTerminal :: !Bool -- ^ 'True', if node is being terminal.
  } deriving (Eq)

instance Show RankedCompleterNode where
  show RankedCompleterNode{..} = intercalate "\t"
    [ show nodeDictIx
    , show prevNodeIx
    , show $ chr $ fromIntegral nodeLabel
    , show nodeIsQueued
    , show nodeHasTerminal
    ]

-- | Empty node.
empty :: RankedCompleterNode
empty = RankedCompleterNode
  { nodeDictIx = 0
  , prevNodeIx = 0
  , nodeLabel = 0
  , nodeIsQueued = False
  , nodeHasTerminal = False
  }
{-# INLINE empty #-}

-- | Sets a dictionary index to the given ranked completer node.
setDictIx :: BaseType -> RankedCompleterNode -> RankedCompleterNode
setDictIx !dictIx !RankedCompleterNode{..} = RankedCompleterNode { nodeDictIx = dictIx, .. }
{-# INLINE setDictIx #-}

-- | Sets a previous node index to the given ranked completer node.
setPrevNodeIx :: BaseType -> RankedCompleterNode -> RankedCompleterNode
setPrevNodeIx !dictIx !RankedCompleterNode{..} =
  RankedCompleterNode { prevNodeIx = dictIx, .. }
{-# INLINE setPrevNodeIx #-}

-- | Sets a label to the given ranked completer node.
setLabel :: UCharType -> RankedCompleterNode -> RankedCompleterNode
setLabel !label !RankedCompleterNode{..} = RankedCompleterNode { nodeLabel = label, .. }
{-# INLINE setLabel #-}

-- | Sets @IS_QUEUED@ flag to the given ranked completer node.
setIsQueued :: Bool -> RankedCompleterNode -> RankedCompleterNode
setIsQueued !isQueued !RankedCompleterNode{..} = RankedCompleterNode { nodeIsQueued = isQueued, .. }
{-# INLINE setIsQueued #-}

-- | Sets @HAS_TERMINAL@ flag to the given ranked completer node.
setHasTerminal :: Bool -> RankedCompleterNode -> RankedCompleterNode
setHasTerminal !hasTerminal !RankedCompleterNode{..} =
  RankedCompleterNode { nodeHasTerminal = hasTerminal, .. }
{-# INLINE setHasTerminal #-}
