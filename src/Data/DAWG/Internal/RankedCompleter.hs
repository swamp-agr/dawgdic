{-|
Module: Data.DAWG.Internal.RankedCompleter
Description: Exports ranked completer as well as its internal API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
{-# LANGUAGE CPP #-}
module Data.DAWG.Internal.RankedCompleter where

import Control.Monad (forM, forM_)
import Control.Monad.ST (runST)
import Data.Bits ((.^.))
import Data.Char (chr, ord)
import Data.Set (Set)
import Data.Vector (Vector)
import GHC.Stack (HasCallStack)

import Data.DAWG.Internal.BaseType
import Data.DAWG.Internal.Dictionary (Dictionary)
import Data.DAWG.Internal.Guide (Guide (..))
import Data.DAWG.Internal.RankedCompleterCandidate
import Data.DAWG.Internal.RankedCompleterNode
import Data.DAWG.Internal.RankedGuide

import qualified Data.DAWG.Internal.Dictionary as Dict
import qualified Data.DAWG.Internal.DictionaryUnit as DU
import qualified Data.DAWG.Internal.RankedGuide as G
import qualified Data.DAWG.Internal.RankedCompleterCandidate as Candidate
import qualified Data.DAWG.Internal.RankedCompleterNode as Node

import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as VM

#ifdef trace
import System.IO.Unsafe
import Data.DAWG.Trace
#endif


-- ** Priority Queue

-- | Priority queue.
data PQ a = PQ
  { pqSet :: !(Set a) -- ^ Underneath the priority queue it is just a 'Data.Set.Set'.
  , pqTop :: !a -- ^ Top value of a queue. Holds a "minimal" value of a set. When set is empty, it holds an empty value as well.
  }

-- | Checks whether priority queue is nullable or not.
nullPQ :: PQ a -> Bool
nullPQ = Set.null . pqSet
{-# INLINE nullPQ #-}

-- | Initialises an empty priority queue.
emptyPQ :: Monoid a => PQ a
emptyPQ = PQ
  { pqSet = Set.empty
  , pqTop = mempty
  }

-- | Gets a top element from the priority queue. /O(1)/.
queueTop :: PQ a -> a
queueTop = pqTop
{-# INLINE queueTop #-}

-- | Removes and discards a minimal element from the priority queue.
queuePop :: Monoid a => PQ a -> PQ a
queuePop !prev =
  let !nset = Set.deleteMin (pqSet prev)
  in if Set.null nset
     then emptyPQ
     else PQ { pqSet = nset, pqTop = Set.findMin nset }
{-# INLINE queuePop #-}

-- | Pushes an element to the queue.
queuePush :: (Ord a) => a -> PQ a -> PQ a
queuePush a prev = if nullPQ prev
  then PQ { pqSet = Set.singleton a, pqTop = a }
  else 
    let ptop = pqTop prev
        ntop = if a < ptop then a else ptop
    in PQ { pqSet = Set.insert a $ pqSet prev, pqTop = ntop }        

-- ** Ranked Completer

-- | Use 'RankedCompleter' to perform completion requests ranking results by values stored in the dictionary. As 'Data.DAWG.Internal.Completer.Completer', it accumulates data during traversing dictionary via ranked guide. Resulted completion could be accessed via 'keyToString' helper. Value is stored in 'rankedCompleterValue'.
data RankedCompleter = RankedCompleter
  { rankedCompleterGuide :: !RankedGuide
  , rankedCompleterKey :: !(Vector UCharType)
  , rankedCompleterPrefixLength :: !BaseType
  , rankedCompleterValue :: !ValueType
  , rankedCompleterNodes :: !(Vector RankedCompleterNode)
  , rankedCompleterNodeQueue :: !(Vector BaseType)
  , rankedCompleterCandidateQueue :: !(PQ RankedCompleterCandidate)
  }

-- | Helper to access a dictionary from 'RankedCompleter'.
rankedCompleterDictionary :: RankedCompleter -> Dictionary
rankedCompleterDictionary = guideDictionary . rankedGuide . rankedCompleterGuide

-- | Retrieves a completion result from 'RankedCompleter' as 'String'.
keyToString :: RankedCompleter -> String
keyToString = fmap (chr . fromIntegral) . safeInit . V.toList . rankedCompleterKey
  where
    safeInit [] = []
    safeInit xs = init xs
    {-# INLINE safeInit #-}
{-# INLINE keyToString #-}

-- | Starts completion process for 'RankedCompleter' with a 'Dictionary' index and word prefix. For basic usage pass @0@ (dictionary 'Data.DAWG.Internal.Dictionary.root' index) as index.
-- Fpr more complex scenarios different 'Dictionary' indexes could be used here too.
start :: HasCallStack => BaseType -> String -> RankedGuide -> RankedCompleter
start !ix !prefix !guide =
    let !gsize = guideSize $ rankedGuide guide
        prefix' = V.map (fromIntegral @_ @UCharType . ord) $ V.fromList prefix
  
        !nc = RankedCompleter
          { rankedCompleterGuide = guide
          , rankedCompleterKey = prefix'
          , rankedCompleterPrefixLength = fromIntegral $ V.length prefix'
          , rankedCompleterValue = -1
          , rankedCompleterNodes = V.empty
          , rankedCompleterNodeQueue = V.empty
          , rankedCompleterCandidateQueue = emptyPQ
          }
    in if gsize /= 0
      then enqueueNode 0 $ snd $ createNode ix 0 (fromIntegral $ ord 'X') nc
      else nc
  
-- | Retrieves next completion. If present, 'RankedCompleter' will be returned. 'Nothing', otherwise.
next :: HasCallStack => RankedCompleter -> Maybe RankedCompleter
#ifdef trace
next rc = unsafePerformIO do
  traceIO $ concat ["-rc:next nq.size ", show $ V.length $ rankedCompleterNodeQueue rc]
#else
next rc =
#endif
  let findSiblingNode !i !c
        | i >= fromIntegral (V.length $ rankedCompleterNodeQueue c) = c
#ifdef trace
        | otherwise = unsafePerformIO do
          let tnodeIx = rankedCompleterNodeQueue c V.! i
          traceIO $ concat [ "-rc:next ix ", show i, " nodeIx ", show tnodeIx]
          dumpC rc
#else
        | otherwise =
#endif
          let !nodeIx = rankedCompleterNodeQueue c V.! i
              (hasFoundSibling, nc, nextNodeIx) = if rankedCompleterValue c == -1
                then (True, c, nodeIx) -- skipping looking up sibling for -1
                else findSibling nodeIx c
#ifdef trace
          if not hasFoundSibling
             then pure $ findSiblingNode (succ i) nc
             else do
                  let (nextNodeIx', nc') = findTerminal nextNodeIx nc
                  traceIO $ concat ["-next:findSiblingNode next-nix ", show nextNodeIx']
                  let nc'' = enqueueCandidate nextNodeIx' nc'
                  dumpC nc''
                  pure $ findSiblingNode (succ i) nc''
#else
          in if not hasFoundSibling
             then findSiblingNode (succ i) nc
             else let (nextNodeIx', nc') = findTerminal nextNodeIx nc
                      nc'' = enqueueCandidate nextNodeIx' nc'
                  in findSiblingNode (succ i) nc''
#endif
      checkCandidate !c =
        let !nc = findSiblingNode 0 c
        in if nullPQ (rankedCompleterCandidateQueue nc)
           then Nothing
           else Just nc { rankedCompleterNodeQueue = V.empty }

      resizeKey !c =
        let !newSize = fromIntegral $ rankedCompleterPrefixLength c
            !oldSize = V.length $ rankedCompleterKey c
            grow !v = do
              mv <- V.unsafeThaw v
              nv <- VM.grow mv (newSize - oldSize)
              forM_ [ oldSize .. pred newSize ] do
                \i -> VM.unsafeWrite nv i 0
              V.unsafeFreeze nv

            slice !v = V.slice 0 newSize v

            !newKey = case compare oldSize newSize of
              EQ -> rankedCompleterKey c
              LT -> runST $ grow $ rankedCompleterKey c
              GT -> slice $ rankedCompleterKey c
        in c { rankedCompleterKey = newKey }

      enqueueNodes :: BaseType -> RankedCompleter -> (BaseType, RankedCompleter)
      enqueueNodes 0 !c = (0, c)
      enqueueNodes !nodeIx !c =
        let !node = rankedCompleterNodes c V.! fromIntegral nodeIx
            !label' = Node.nodeLabel node
            !newKey = pushBack label' $ rankedCompleterKey c
            !nc = c { rankedCompleterKey = newKey }
            !nc' = enqueueNode nodeIx nc
            !nextNodeIx = Node.prevNodeIx node
        in enqueueNodes nextNodeIx nc'

      postProcessKey !c =
        let reverse' !v = do
              let startIx = 0 + (fromIntegral $ rankedCompleterPrefixLength c)
                  endIx = pred $ V.length $ rankedCompleterKey c
                  indexes = [ startIx .. endIx ]
              values <- forM indexes \i -> do
                VM.unsafeRead v i
              forM_ (zip indexes (reverse values)) \(ix, value') -> do
                VM.unsafeWrite v ix value'
        in pushBack 0 $ V.modify reverse' $ rankedCompleterKey c
      
      processCandidate !c =
        let !candidate = queueTop (rankedCompleterCandidateQueue c)
            !nodeIx' = candidateNodeIx candidate
            !nc = enqueueNode nodeIx' c
            !nodeIx'' = Node.prevNodeIx (rankedCompleterNodes nc V.! fromIntegral nodeIx')
            (!_, !nc') = enqueueNodes nodeIx'' $ resizeKey nc
        in nc'
           { rankedCompleterKey = postProcessKey nc'
           , rankedCompleterValue = candidateValue candidate
           , rankedCompleterCandidateQueue = queuePop (rankedCompleterCandidateQueue nc')
           }
#ifdef trace
  pure $! case checkCandidate rc of
#else
  in case checkCandidate rc of
#endif
    Nothing -> Nothing
    Just rcc -> Just $! processCandidate rcc

-- | Retrieves a value associated
-- with the last visited index by 'RankedCompleter' from the 'Dictionary'.
value :: RankedCompleter -> ValueType
value = rankedCompleterValue
{-# INLINE value #-}

-- | Creates a new node and stores it inside 'RankedCompleter'. Returns node identifier and new state of 'RankedCompleter'.
createNode
  :: BaseType -> BaseType -> UCharType -> RankedCompleter -> (BaseType, RankedCompleter)
#ifdef trace
createNode !dictIx !prevNodeIx' !label !c = unsafePerformIO do
  traceIO $ concat ["---createNode dictIx ", show dictIx, " prev_nix ", show prevNodeIx', " l ", show label]
#else
createNode !dictIx !prevNodeIx' !label !c = 
#endif
  let !dict = rankedCompleterDictionary c
      maybeSetTerminal x = if label == 0
        then x
        else Node.setHasTerminal (Dict.hasValue dictIx dict) x
      node = maybeSetTerminal
        $ Node.setLabel label
        $ Node.setPrevNodeIx prevNodeIx'
        $ Node.setDictIx dictIx
        $ Node.empty
      nextNodes = pushBack node (rankedCompleterNodes c)
#ifdef trace
  pure $! (fromIntegral $ V.length nextNodes - 1, c { rankedCompleterNodes = nextNodes })
#else
  in (fromIntegral $ V.length nextNodes - 1, c { rankedCompleterNodes = nextNodes })
#endif

-- | Pushes a node to queue.
enqueueNode :: BaseType -> RankedCompleter -> RankedCompleter
enqueueNode !nodeIx !c
  | nodeIsQueued ((rankedCompleterNodes c) V.! fromIntegral nodeIx) = c
  | otherwise =
      let modifyIsQueued !v = do
            let nix' = fromIntegral nodeIx
            node <- VM.unsafeRead v nix'
            VM.unsafeWrite v nix' $ Node.setIsQueued True node
          !nnq = pushBack nodeIx (rankedCompleterNodeQueue c)
          !ns = V.modify modifyIsQueued (rankedCompleterNodes c)
      in c { rankedCompleterNodeQueue = nnq
           , rankedCompleterNodes = ns
           }

-- | Pushes a candidate to the priority queue.
enqueueCandidate :: HasCallStack => BaseType -> RankedCompleter -> RankedCompleter
#ifdef trace
enqueueCandidate !nodeIx !c = unsafePerformIO do
  let !_dictIx = Node.nodeDictIx (rankedCompleterNodes c V.! fromIntegral nodeIx)
  traceIO $ concat ["---enqueueCandidate dictIx ", show _dictIx]
  let !_dictValue = DU.value
        $ Dict.dictionaryUnits (rankedCompleterDictionary c) UV.! fromIntegral _dictIx
  traceIO $ concat ["---enqueueCandidate dictValue ", show _dictValue]
#else
enqueueCandidate !nodeIx !c =
#endif
  let !dictIx = Node.nodeDictIx (rankedCompleterNodes c V.! fromIntegral nodeIx)
      !dictValue = DU.value
        $ Dict.dictionaryUnits (rankedCompleterDictionary c) UV.! fromIntegral dictIx
      !candidate = Candidate.setValue dictValue
        $ Candidate.setNodeIx nodeIx
        $ Candidate.empty
      !nextCQueue = queuePush candidate (rankedCompleterCandidateQueue c)
#ifdef trace
  traceIO $ concat ["---enqueueCandidate nix ", show nodeIx, " v ", show dictValue]
  pure $! c { rankedCompleterCandidateQueue = nextCQueue }
#else
  in c { rankedCompleterCandidateQueue = nextCQueue }
#endif

-- | Finds a sibling for given node index. Returns:
--
-- * Flag indicating whether sibling has found or not.
-- * Updated 'RankedCompleter'.
-- * Next node indentifier.
--
-- If sibling has not found, returns @(False, original completer, original node index)@.
findSibling :: BaseType -> RankedCompleter -> (Bool, RankedCompleter, BaseType)
#ifdef trace
findSibling !nodeIx !c = unsafePerformIO do
  traceIO $ concat ["--findSibling nix ", show nodeIx]
  dumpC c
#else
findSibling !nodeIx !c =
#endif
  let !node = (rankedCompleterNodes c) V.! fromIntegral nodeIx
      !prevNodeIx' = Node.prevNodeIx node
      !dictIx = Node.nodeDictIx node
      !siblingLabel = G.sibling dictIx $ rankedCompleterGuide c
      mChangedNodes = if siblingLabel == 0
        then let !prevNode = (rankedCompleterNodes c) V.! fromIntegral prevNodeIx'
             in if not (Node.nodeHasTerminal prevNode)
                then Nothing
                else let modifyHasTerminal !v = do
                           let ix' = fromIntegral prevNodeIx'
                           node' <- VM.unsafeRead v ix'
                           VM.unsafeWrite v ix' $ Node.setHasTerminal False node'
                         !ns = V.modify modifyHasTerminal (rankedCompleterNodes c)
                     in Just ns
        else Just (rankedCompleterNodes c)
#ifdef trace
  traceIO $ concat
    [ "--findSibling nix ", show nodeIx
    , " prev_nix ", show prevNodeIx'
    , " dictIx ", show dictIx
    , " sib ", show siblingLabel
    ]
  case mChangedNodes of
    Nothing -> pure (False, c, nodeIx)
    Just nodes -> do
#else
  in case mChangedNodes of
    Nothing -> (False, c, nodeIx)
    Just nodes ->
#endif
      let !dictPrevIx = Node.nodeDictIx (nodes V.! fromIntegral prevNodeIx')
          !dict = rankedCompleterDictionary c
          !nextDictIx = followWithoutCheck dictPrevIx siblingLabel dict
          nc = c { rankedCompleterNodes = nodes }
          (nextNodeIx, nc') = createNode nextDictIx prevNodeIx' siblingLabel nc

#ifdef trace
      pure (True, nc', nextNodeIx)
#else
      in (True, nc', nextNodeIx)
#endif
          
-- | Recursively finds a terminal node for given node index.
findTerminal :: BaseType -> RankedCompleter -> (BaseType, RankedCompleter)
findTerminal !nodeIx !c
  | Node.nodeLabel (rankedCompleterNodes c V.! fromIntegral nodeIx) == 0 = (nodeIx, c)
#ifdef trace
  | otherwise = unsafePerformIO do
    let !dnode = rankedCompleterNodes c V.! fromIntegral nodeIx
        !ddictIx = Node.nodeDictIx dnode
        !dchildLabel = G.child ddictIx $ rankedCompleterGuide c
    traceIO $ concat ["--findTerminal nix ", show nodeIx, " cl ", show dchildLabel, " dictIx ", show ddictIx]
#else
  | otherwise =
#endif
    let !node = rankedCompleterNodes c V.! fromIntegral nodeIx
        !dictIx = Node.nodeDictIx node
        !childLabel = G.child dictIx $ rankedCompleterGuide c
        !nodes = if childLabel == 0
          then let modifyHasTerminal !v = do
                     let ix' = fromIntegral nodeIx
                     node' <- VM.unsafeRead v ix'
                     VM.unsafeWrite v ix' $ Node.setHasTerminal False node'
               in V.modify modifyHasTerminal (rankedCompleterNodes c)
          else rankedCompleterNodes c
        !dict = rankedCompleterDictionary c
        !nextDictIx = followWithoutCheck dictIx childLabel dict
        nc = c { rankedCompleterNodes = nodes }
        (!nextNodeIx, nc') = createNode nextDictIx nodeIx childLabel nc
#ifdef trace
    traceIO $ concat ["--findTerminal next-nix ", show nextNodeIx, " nextDictIx ", show nextDictIx]
    dumpC nc'
    pure $! findTerminal nextNodeIx nc'
#else
    in findTerminal nextNodeIx nc'
#endif

-- | Follows label in the dictionary without checking for offsets.
followWithoutCheck :: BaseType -> UCharType -> Dictionary -> BaseType
followWithoutCheck !ix !label !d =
  (ix .^. DU.offset (Dict.dictionaryUnits d UV.! fromIntegral ix)) .^. fromIntegral label

-- | Helper that pushes element to the end of the array.
pushBack :: a -> Vector a -> Vector a
pushBack a as = runST $ snoc as
  where
    n = V.length as
    snoc v = do
      mv <- V.unsafeThaw v
      nv <- VM.grow mv 1
      VM.unsafeWrite nv n a
      V.unsafeFreeze nv

#ifdef trace
dumpC :: VM.PrimMonad m => RankedCompleter -> m ()
dumpC RankedCompleter{..} = do
  traceIO "------- BEGIN -------"
  traceIO $ concat ["prefix_length : ", show rankedCompleterPrefixLength]
  traceIO $ concat ["value : ", show rankedCompleterValue]
  dumpVector "key" rankedCompleterKey
  dumpVector "nodes" rankedCompleterNodes
  dumpVector "node_queue" rankedCompleterNodeQueue
  traceIO $ concat ["candidate_queue : top : ", show $ pqTop rankedCompleterCandidateQueue]
  dumpVector "candidate_queue : values" $ V.fromList $ Set.toList $ pqSet rankedCompleterCandidateQueue
  traceIO "------- END -------"

dumpVector :: (VM.PrimMonad m, Show a) => String -> Vector a -> m ()
dumpVector label v = do
  traceIO label
  let l = V.length v
  traceIO $ concat ["i(", show l, ")\tv"]
  forM_ [ 0 .. pred l ] \ix -> do
    let u = v V.! ix
    traceIO $ concat [show ix, "\t", show u]
#endif
