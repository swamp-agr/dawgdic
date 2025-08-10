{-|
Module: Data.DAWG.Internal.DictionaryBuilder
Description: Exports dictionary builder as well as its internal API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
{-# LANGUAGE CPP #-}
module Data.DAWG.Internal.DictionaryBuilder where

import Control.Monad (forM_, when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Primitive.MutVar
import GHC.Stack (HasCallStack)

import Data.Primitive.PrimArray.Combinators
import Data.DAWG.Internal.BaseType (BaseType, UCharType, SizeType, UUHT, UHHT, ObjectPool)
import Data.DAWG.Internal.DictionaryExtraUnit (DictionaryExtraUnit (..))
import Data.DAWG.Internal.DictionaryUnit (DictionaryUnit (..))
import Data.DAWG.Internal.Dictionary (Dictionary (..))
import Data.DAWG.Internal.DAWG (DAWG (..))

import qualified Data.Primitive.PrimArray.Utils as A
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Hashtables as HT

import qualified Data.DAWG.Internal.DAWG as Dawg
import qualified Data.DAWG.Internal.DictionaryExtraUnit as Extra
import qualified Data.DAWG.Internal.DictionaryUnit as DictUnit
import qualified Data.DAWG.Internal.LinkTable as LT

#ifdef trace
import Data.DAWG.Trace
#endif

-- ** DAWG Dictionary Builder

-- | A mutable builder of 'Data.DAWG.Internal.Dictionary.Dictionary'.
newtype DictionaryBuilder m =
  DDBRef { getDDBRef :: MutVar (PrimState m) (DictionaryBuilder_ m) }

-- | Builder of Dictionary. Do not access directly. Use 'DictionaryBuilder' instead.
data DictionaryBuilder_ m = DictionaryBuilder
  { dawgDictionaryBuilderDawg :: DAWG -- ^ DAWG.
  , dawgDictionaryBuilderUnits :: ObjectPool (PrimState m) DictionaryUnit -- ^ Pool of dictionary units.
  , dawgDictionaryBuilderExtras :: UHHT m BaseType DictionaryExtraUnit -- ^ Table of extra blocks (of 256) which represents supportive circular linked list used.
  , dawgDictionaryBuilderLabels :: UUHT m SizeType UCharType
  , dawgDictionaryBuilderLinkTable :: LT.LinkTable m
  , dawgDictionaryBuilderRefs :: !(IntArray (PrimState m))
  }

unfixedIndex, numOfUnusedUnits :: Int

-- | Use it as index for 'dawgDictionaryBuilderRefs' to get the current unfixed index.
unfixedIndex = 0
{-# INLINE unfixedIndex #-}

-- | Use it as index for 'dawgDictionaryBuilderRefs' to get the number of unfixed units.
numOfUnusedUnits = 1
{-# INLINE numOfUnusedUnits #-}

-- | Upper mask.
upperMask :: BaseType
upperMask = complement (pred DictUnit.offsetMax)
{-# INLINE upperMask #-}

-- | Lower mask.
lowerMask :: BaseType
lowerMask = 0xFF
{-# INLINE lowerMask #-}

-- | Gets a current size of units.
numOfUnits :: DictionaryBuilder_ m -> BaseType
numOfUnits = fromIntegral . V.length . dawgDictionaryBuilderUnits
{-# INLINE numOfUnits #-}

-- | Gets a current size of blocks.
numOfBlocks :: PrimMonad m => DictionaryBuilder_ m -> m BaseType
numOfBlocks = fmap fromIntegral . HT.size . dawgDictionaryBuilderExtras
{-# INLINE numOfBlocks #-}

-- | Constant: @16@.
numOfUnfixedBlocks :: BaseType
numOfUnfixedBlocks = 16
{-# INLINE numOfUnfixedBlocks #-}

-- | Constant: @256@.
blockSize :: BaseType
blockSize = 256
{-# INLINE blockSize #-}

-- | Build dictionary o
build
  :: HasCallStack
  => PrimMonad m
  => DAWG -> m (Maybe (DictionaryBuilder m))
build !dawg = do
  !dref@DDBRef{..} <- new dawg
  !preDdb <- readMutVar getDDBRef
  let !ltsize = dawgNumOfMergingStates dawg + (dawgNumOfMergingStates dawg .>>. 1)
#ifdef trace
  traceIO ("build ltsize " <> show ltsize)
#endif
  LT.init (dawgDictionaryBuilderLinkTable preDdb) (fromIntegral ltsize)

#ifdef trace
  traceIO ("build reserveUnit 0")
  traceWith dump dref
#endif

  reserveUnit 0 dref
#ifdef trace
  traceWith dump dref
#endif
  -- after unit reservation most likely vectors are being resized
  !ddb <- readMutVar getDDBRef
  let units = dawgDictionaryBuilderUnits ddb

#ifdef trace
  traceIO ("build extra setIsUsed 0")
#endif
  modifyExtras ddb 0 $ Extra.setIsUsed
  !u0 <- units !~  0
#ifdef trace
  traceIO ("build get unit[0] " <> show u0)
#endif
  let (!isOffsetSet, !u1) = DictUnit.setOffset 1 u0
  if not isOffsetSet
    then do
#ifdef trace
      traceIO $ "build: offset is not set for " <> show u0
#endif
      pure Nothing
    else do
#ifdef trace
      traceIO ("build set offset unit[0] " <> show u1)
#endif
      let !u2 =  DictUnit.setLabel (fromIntegral $ ord '\0') u1
#ifdef trace
      traceIO ("build set label unit[0] " <> show u2)
#endif
      units <~~ 0 $ u2

      buildResult <- if (Dawg.size dawg > 1)
        then do
#ifdef trace
          traceIO ("build from dawg from 0")
#endif
          buildFromDawg Dawg.root 0 dref
        else pure True

#ifdef trace
      traceIO ("build result: " <> show buildResult)
#endif
      if not buildResult
        then pure Nothing
        else do
#ifdef trace
          traceIO "build fixAllBlocks"
#endif
          fixAllBlocks dref
#ifdef trace
          traceWith dump dref
#endif
          pure $! Just dref

-- | Build a dictionary from 'Data.DAWG.Internal.DAWG.DAWG' and freezes its result.
-- Throws an error when build fails.
build' :: HasCallStack => PrimMonad m => DAWG -> m Dictionary
build' dawg = build dawg >>= \case
  Just dict -> freeze dict
  Nothing -> error "failed to build dictionary"
{-# INLINE build' #-}

-- | Generates 'Data.DAWG.Internal.Dictionary.Dictionary' out of 'DictionaryBuilder'.
-- Once this function is called, 'DictionaryBuilder' must not be used anymore.
freeze :: PrimMonad m => DictionaryBuilder m -> m Dictionary
freeze DDBRef{..} = do
  ddb <- readMutVar getDDBRef
  dictionaryUnits <- VG.unsafeFreeze $! dawgDictionaryBuilderUnits ddb
  let dictionarySize = fromIntegral $! VG.length dictionaryUnits
  pure Dictionary{..}
{-# INLINE freeze #-}

-- ** Helpers

-- | Initialises a new 'DictionaryBuilder' from DAWG.
new :: HasCallStack => PrimMonad m => DAWG -> m (DictionaryBuilder m)
new !dawgDictionaryBuilderDawg = do
   !dawgDictionaryBuilderUnits <- V.new 0
   !dawgDictionaryBuilderExtras <- HT.initialize 0
   !dawgDictionaryBuilderLabels <- HT.initialize 0
   !dawgDictionaryBuilderLinkTable <- HT.initialize 0
   !dawgDictionaryBuilderRefs <- A.replicate 2 0
   dawgDictionaryBuilderRefs <~ unfixedIndex $ 0
#ifdef trace
   !uix <- dawgDictionaryBuilderRefs ! unfixedIndex
   traceIO ("new dictionary builder: uix " <> show uix)
#endif
   let d = DictionaryBuilder{..}
   DDBRef <$> newMutVar d
{-# INLINE new #-}

-- | Recursively build dictionary by traversing DAWG
-- starting from dawg index and dictionary index.
buildFromDawg
  :: HasCallStack
  => PrimMonad m => BaseType -> BaseType -> DictionaryBuilder m -> m Bool
buildFromDawg dawgIx dictIx dref@DDBRef{..} = do
#ifdef trace
  traceIO ("buildFromDawg dawgIx " <> show dawgIx <> " dictIx " <> show dictIx)
  traceWith dump dref
#endif

  ddb <- readMutVar getDDBRef
  let dawg = dawgDictionaryBuilderDawg ddb
  if Dawg.isLeaf dawgIx dawg
    then pure True
    else do
      let !dawgChildIx = Dawg.child dawgIx dawg
          whenMerging !ix action = do
#ifdef trace
            traceIO $ concat
              ["-whenMerging dawgChildIx ", show ix, " ", show $ Dawg.isMerging ix dawg]
#endif
            if not (Dawg.isMerging ix dawg)
            then pure Nothing
            else action

          withOffset !ix action = do
            !offset <- LT.find (dawgDictionaryBuilderLinkTable ddb) ix
#ifdef trace
            traceIO $ concat ["--withOffset dawgChildIx ", show ix, " offset ", show offset]
#endif
            if offset /= 0
              then action ix offset
              else pure Nothing

          withRenewedOffset !ix !offset = do
            let !renewedOffset = offset .^. dictIx
            if 0 == (renewedOffset .&. upperMask)
              || 0 == (renewedOffset .&. lowerMask)
              then do
                when (Dawg.isLeaf ix dawg) do
                  dawgDictionaryBuilderUnits ddb !<~~ dictIx $! DictUnit.setHasLeaf
                !u <- dawgDictionaryBuilderUnits ddb !~ dictIx
                let (!_isSet, !nu) = DictUnit.setOffset renewedOffset u
                dawgDictionaryBuilderUnits ddb <~~ dictIx $ nu
                pure $! Just True
              else pure Nothing

      whenMerging dawgChildIx (withOffset dawgChildIx withRenewedOffset) >>= \case
        Just x -> pure x
        Nothing -> do
          offset <- arrangeChildNodes dawgIx dictIx dref
          if offset == 0
            then pure False
            else do
              when (Dawg.isMerging dawgChildIx dawg) do
                LT.insert (dawgDictionaryBuilderLinkTable ddb) dawgChildIx offset
              let go !ix
                    | ix == 0 = pure True
                    | otherwise = do
                        let !l = Dawg.label ix dawg
                            !dictChildIx = offset .^. fromIntegral @_ @BaseType l
#ifdef trace
                        traceIO $ concat
                          [ "--go ix ", show ix
                          , " dictChildIx ", show dictChildIx
                          , " offset ", show offset
                          ]
#endif
                        !buildResult <- buildFromDawg ix dictChildIx dref
                        if not buildResult
                          then pure False
                          else do
                            let !nextIx = Dawg.sibling ix dawg
                            go nextIx

              go dawgChildIx

-- | Arrange child nodes for given dawg index and dictionary index.
arrangeChildNodes
  :: HasCallStack
  => PrimMonad m
  => BaseType -> BaseType -> DictionaryBuilder m -> m BaseType
arrangeChildNodes dawgIx dictIx dref@DDBRef{..} = do
  clearLabels dref
  ddb <- readMutVar getDDBRef

  labelSizeRef <- newMutVar (0 :: SizeType)

  let dawg = dawgDictionaryBuilderDawg ddb
      !dawgChildIx = Dawg.child dawgIx dawg

      collectChildLabels 0 = pure ()
      collectChildLabels !ix = do
#ifdef trace
        traceIO ("-collectChildLabels ix " <> show ix)
#endif
        l <- readMutVar labelSizeRef
        HT.insert (dawgDictionaryBuilderLabels ddb) l (Dawg.label ix dawg)
        modifyMutVar' labelSizeRef succ
        let !childIx = Dawg.sibling ix dawg
        collectChildLabels childIx

#ifdef trace
  traceIO $ concat
    [ "arrangeChildNodes dawgIx ", show dawgIx
    , " dawgChildIx ", show dawgChildIx
    , " dictIx ", show dictIx
    ]
#endif
  -- Arrange child nodes.
  collectChildLabels dawgChildIx

  -- Find a good offset.
  !offset <- findGoodOffset dictIx ddb
#ifdef trace
  traceIO $ concat [ "arrangeChildNodes dictIx ", show dictIx, " offset ", show offset]
#endif
  !offsetIsSet <- do
    !u <- dawgDictionaryBuilderUnits ddb !~ dictIx
    let (res, nu) = DictUnit.setOffset (dictIx .^. offset) u
    dawgDictionaryBuilderUnits ddb <~~ dictIx $ nu
    pure res
  if not offsetIsSet
    then pure 0
    else do
      l <- readMutVar labelSizeRef
      let populateChildNodes !i dawgChildIx'
            | i < fromIntegral l = do
                ddb1 <- readMutVar getDDBRef
                label <- fromMaybe 0 <$> HT.lookup (dawgDictionaryBuilderLabels ddb1) i
                let !dictChildIx = offset .^. fromIntegral @_ @BaseType label
                reserveUnit dictChildIx dref
                ddb2 <- readMutVar getDDBRef

#ifdef trace
                traceIO $ concat
                  [ "-populateChildNodes i ", show i
                  , " dawgChildIx ", show dawgChildIx'
                  , " dawg_is_leaf ", show $ Dawg.isLeaf dawgChildIx' dawg
                  , " dawg_value ", show $ Dawg.value dawgChildIx' dawg
                  , " dictIx ", show dictIx, " dictChildIx ", show dictChildIx
                  ]
#endif
                if Dawg.isLeaf dawgChildIx' dawg
                  then do
                    dawgDictionaryBuilderUnits ddb2 !<~~ dictIx $ DictUnit.setHasLeaf
                    dawgDictionaryBuilderUnits ddb2 !<~~ dictChildIx $
                      DictUnit.setValue $! Dawg.value dawgChildIx' dawg
                   
                  else do
                    dawgDictionaryBuilderUnits ddb2 !<~~ dictChildIx $
                      DictUnit.setLabel label

                let !nextDawgChildIx = Dawg.sibling dawgChildIx' dawg
                populateChildNodes (succ i) nextDawgChildIx
            | otherwise = pure ()
      populateChildNodes 0 dawgChildIx
      ddb3 <- readMutVar getDDBRef
      modifyExtras ddb3 offset $! Extra.setIsUsed
      
      pure offset

-- | Find a good offset for given dictionary index.
findGoodOffset
  :: HasCallStack
  => PrimMonad m => BaseType -> DictionaryBuilder_ m -> m BaseType
findGoodOffset ix ddb = do
  !unfixedIndex' <- fromIntegral <$> dawgDictionaryBuilderRefs ddb ! unfixedIndex
  let !numOfUnits' = numOfUnits ddb
#ifdef trace
  traceIO $ concat
    [ "-findGoodOffset ix ", show ix
    , " uix ", show unfixedIndex'
    , " num_of_units ", show numOfUnits'
    ]
#endif
  if numOfUnits' <= unfixedIndex'
    then pure $ numOfUnits' .|. (ix .&. lowerMask)
    else do
      let scanUnusedUnits shouldStop !uix
            | shouldStop && uix == unfixedIndex' =
                pure (numOfUnits' .|. (ix .&. lowerMask))
            | otherwise = do
                l0 <- fromMaybe 0 <$> HT.lookup (dawgDictionaryBuilderLabels ddb) 0
                let !offset = uix .^. fromIntegral l0
#ifdef trace
                traceIO $ concat
                  [ "--scanUnusedUnits ix ", show ix
                  , " uix ", show uix
                  , " l ", show $ chr $ fromIntegral l0, " (", show l0, ")"
                  , " offset ", show offset
                  ]
#endif
                isGoodOffset ix offset ddb >>= \case
                  True -> pure offset
                  False -> do
                    !ex <- extras ddb (fromIntegral uix)
                    let !nuix = Extra.next ex
                    scanUnusedUnits True nuix
      scanUnusedUnits False unfixedIndex'
{-# INLINE findGoodOffset #-}

-- | Recursively checks whether given offset is good for dictionanry index.
isGoodOffset
  :: HasCallStack
  => PrimMonad m => BaseType -> BaseType -> DictionaryBuilder_ m -> m Bool
isGoodOffset ix offset ddb = do
  !extra' <- extras ddb offset
  if Extra.isUsed extra' then pure False else do
    let !relativeOffset = ix .^. offset
    if (relativeOffset .&. lowerMask /= 0) && (relativeOffset .&. upperMask /= 0)
      then pure False else do
        lsize <- HT.size (dawgDictionaryBuilderLabels ddb)
        let findCollision !i
              | i >= lsize = pure True
              | otherwise = do
                  l <- fromMaybe 0 <$> HT.lookup (dawgDictionaryBuilderLabels ddb) (fromIntegral i)
                  !ex' <- extras ddb (offset .^. fromIntegral @_ @BaseType l)
                  if Extra.isFixed ex'
                    then pure False
                    else findCollision (succ i)
        findCollision 1
{-# INLINE isGoodOffset #-}

-- | Reserve a new unit.
reserveUnit
  :: HasCallStack
  => PrimMonad m => BaseType -> DictionaryBuilder m -> m ()
reserveUnit ix dref = do
  do
    !ddb0 <- readMutVar (getDDBRef dref)
#ifdef trace
    !unfixedIndex' <- dawgDictionaryBuilderRefs ddb0 ! unfixedIndex
    traceIO $ concat
      [ "-reserveUnit ix ", show ix
      , " num_of_units ", show (numOfUnits ddb0)
      , " uix ", show unfixedIndex'
      ] 
#endif
    when (numOfUnits ddb0 <= ix) do
      expandDictionary dref

  -- removes an unused unit from a circular linked list
  !ddb <- readMutVar (getDDBRef dref)
  !unfixedIndex' <- dawgDictionaryBuilderRefs ddb ! unfixedIndex

  when (ix == fromIntegral unfixedIndex') do
    ex' <- extras ddb ix
    let !nextUnfixedIx = Extra.next ex'
    dawgDictionaryBuilderRefs ddb <~ unfixedIndex $ fromIntegral nextUnfixedIx
    when (nextUnfixedIx == ix) do
      dawgDictionaryBuilderRefs ddb <~ unfixedIndex $ fromIntegral $ numOfUnits ddb

  !ex' <- extras ddb ix
  let !next' = Extra.next ex'
      !prev' = Extra.prev ex'

  modifyExtras ddb prev' $! Extra.setNext next'
  modifyExtras ddb next' $! Extra.setPrev prev'
  modifyExtras ddb ix $! Extra.setIsFixed
{-# INLINE reserveUnit #-}

-- | Expands dictionary by allocating a memory for new unit and block and aligning block elements.
expandDictionary :: HasCallStack => PrimMonad m => DictionaryBuilder m -> m ()
expandDictionary dref@DDBRef{..} = do
  (srcNumOfUnits, srcNumOfBlocks, destNumOfUnits, destNumOfBlocks) <- do
    !ddb <- readMutVar getDDBRef
    numOfBlocks' <- numOfBlocks ddb
    let !srcNumOfUnits = numOfUnits ddb
        !srcNumOfBlocks = numOfBlocks'

        !destNumOfUnits = srcNumOfUnits + blockSize
        !destNumOfBlocks = succ srcNumOfBlocks

#ifdef trace
    traceIO $ concat
      [ "--expandDictionary src_u ", show srcNumOfUnits
      , " src_b ", show srcNumOfBlocks
      , " dest_u ", show destNumOfUnits
      , " dest_b ", show destNumOfBlocks
      ]
#endif
  -- Fix old block
    when (numOfUnfixedBlocks < destNumOfBlocks) do
      fixBlock (srcNumOfBlocks - numOfUnfixedBlocks) dref

    -- dest - src
    !newUnits <- V.grow (dawgDictionaryBuilderUnits ddb)
      (fromIntegral blockSize)
    forM_ [srcNumOfUnits .. destNumOfUnits - 1] \ix -> do
      newUnits <~~ ix $ 0

    allocateExtras ddb destNumOfBlocks
    let extras' = dawgDictionaryBuilderExtras ddb
        !ddb' = ddb
          { dawgDictionaryBuilderUnits = newUnits
          , dawgDictionaryBuilderExtras = extras'
          }

    writeMutVar getDDBRef ddb'
    pure (srcNumOfUnits, srcNumOfBlocks, destNumOfUnits, destNumOfBlocks)

  !ddb1 <- readMutVar getDDBRef

  if numOfUnfixedBlocks < destNumOfBlocks
    then do
      numOfBlocks' <- numOfBlocks ddb1
      let !blockId = srcNumOfBlocks - numOfUnfixedBlocks
          !lastId = numOfBlocks' - 1

      swapBlocks ddb1 blockId lastId
      forM_ [srcNumOfUnits .. pred destNumOfUnits] \i -> do
        modifyExtras ddb1 i $! const Extra.empty

    else do
      numOfBlocks' <- numOfBlocks ddb1
      let !lastId = numOfBlocks' - 1
      clearBlock lastId ddb1

#ifdef trace
  extrasSize <- numOfBlocks ddb1
  traceIO $ concat
    [ "--expandDictionary units new size "
    , show (V.length (dawgDictionaryBuilderUnits ddb1))
    , " blocks new size ", show extrasSize]

#endif
  -- create a circular linked list for a new block
  !ddb2 <- readMutVar getDDBRef

  let setNeighbourBlocks !i = do
        modifyExtras ddb2 (pred i) $ Extra.setNext i
        modifyExtras ddb2 i $ Extra.setPrev (pred i)

  forM_ [succ srcNumOfUnits .. pred destNumOfUnits] setNeighbourBlocks
#ifdef trace
  traceIO ("---setNeighbourBlocks " <> show destNumOfUnits)
#endif
  !unfixedIndex' <- dawgDictionaryBuilderRefs ddb2 ! unfixedIndex
  let !uix = fromIntegral unfixedIndex'
#ifdef trace
  traceIO ("--expandDictionary uix " <> show uix)
#endif
  modifyExtras ddb2  srcNumOfUnits $ Extra.setPrev (pred destNumOfUnits)
  modifyExtras ddb2 (pred destNumOfUnits) $ Extra.setNext srcNumOfUnits

  -- Merge 2 circular linked lists
  unfixedIndexBlock <- extras ddb2 uix
  modifyExtras ddb2 srcNumOfUnits $ Extra.setPrev (Extra.prev unfixedIndexBlock)
  modifyExtras ddb2 (pred destNumOfUnits) $ Extra.setNext uix

  modifyExtras ddb2 (Extra.prev unfixedIndexBlock) $ Extra.setNext srcNumOfUnits
  modifyExtras ddb2 uix $ Extra.setPrev (pred destNumOfUnits)
  writeMutVar getDDBRef ddb2

-- | Fixes all blocks. If there is more than 16 blocks, only unfixed blocks will be fixed.
fixAllBlocks
  :: HasCallStack
  => PrimMonad m
  => DictionaryBuilder m -> m ()
fixAllBlocks dref@DDBRef{..} = do
#ifdef trace
  traceIO "-fixAllBlocks"
  traceWith dump dref
#endif
  ddb <- readMutVar getDDBRef
  numOfBlocks' <- numOfBlocks ddb
  let !begin = if numOfUnfixedBlocks < numOfBlocks'
        then numOfBlocks' - numOfUnfixedBlocks
        else 0
      !end = numOfBlocks'
#ifdef trace
  traceIO $ concat ["-fixAllBlocks begin ", show begin, " end ", show end]
#endif
  forM_ [begin .. pred end] \blockId -> do
    fixBlock blockId dref
{-# INLINE fixAllBlocks #-}

-- | Fix block by its id.
fixBlock
  :: HasCallStack
  => PrimMonad m => BaseType -> DictionaryBuilder m -> m ()
fixBlock blockId dref@DDBRef{..} = do
#ifdef trace
  traceIO $ concat [ "-fixBlock block ", show blockId ]
#endif
  ddb <- readMutVar getDDBRef
  let !begin = blockId * blockSize
      !end = begin + blockSize

      findUnusedOffsetForLabel !offset
        | offset /= end = do
            block <- extras ddb offset
            if not $ Extra.isUsed block
              then pure offset
              else findUnusedOffsetForLabel (succ offset)
        | otherwise = pure 0

  offset <- findUnusedOffsetForLabel begin
#ifdef trace
  traceIO $ concat [ "-fixBlock offset ", show offset ]
#endif

  -- Labels of unused units are modified
  let go !ix
        | ix /= end = do
            ddb1 <- readMutVar getDDBRef
#ifdef trace
            ex' <- extras ddb1 ix
            traceIO $ concat [ "-fixBlock ix ", show ix, " e ", show ex' ]
#endif
            Extra.isFixed <$> extras ddb1 ix >>= \case
              True -> pure ()
              False -> do
                reserveUnit ix dref
                ddb2 <- readMutVar getDDBRef
                dawgDictionaryBuilderUnits ddb2 !<~~ ix $ DictUnit.setLabel $!
                  (fromIntegral @_ @UCharType $! ix .^. fromIntegral offset)
                numUnusedUnits' <-  dawgDictionaryBuilderRefs ddb2 ! numOfUnusedUnits
                dawgDictionaryBuilderRefs ddb2 <~ numOfUnusedUnits $ succ numUnusedUnits'
            go (succ ix)
        | otherwise = pure ()
  go begin

-- | Remove all labels.
clearLabels :: PrimMonad m => DictionaryBuilder m -> m ()
clearLabels DDBRef{..} = do
  ddb <- readMutVar getDDBRef
  lkeys <- HT.keys (dawgDictionaryBuilderLabels ddb)
  VG.forM_ lkeys \label -> HT.delete (dawgDictionaryBuilderLabels ddb) label
{-# INLINE clearLabels #-}

-- | Dump dictionary builder to stdout.
dump :: DictionaryBuilder IO -> IO ()
dump DDBRef{..} = do
  ddb <- readMutVar getDDBRef

  !bs <- fromIntegral <$> numOfBlocks ddb
  !ls <- HT.size $ dawgDictionaryBuilderLabels ddb
  let !us = V.length $ dawgDictionaryBuilderUnits ddb
      !ms = maximum [us, bs * fromIntegral blockSize, ls]
      labelToString x = concat [ show $ chr $ fromIntegral x, " (", show x, ")" ]

  putStrLn $ concat [ "i\tu(", show us, ")\t\tb(", show bs, ")\t\t\tl(", show ls, ")"]

  forM_ [0 .. ms - 1] \i -> do
    !u <- maybe "" show <$> (V.readMaybe (dawgDictionaryBuilderUnits ddb) i)
    !b <- do
        b' <- extras ddb (fromIntegral i)
        if b' == Extra.empty then pure "" else pure $ show b'
    !l <-  maybe "" labelToString <$> (HT.lookup (dawgDictionaryBuilderLabels ddb) $ fromIntegral i)

    when (any (/= mempty) [b, l] || u /= show DictUnit.empty) do
      putStrLn $ concat [ show i, "\t", u, "\t", b, "\t", l  ]

  uix <- dawgDictionaryBuilderRefs ddb ! unfixedIndex
  uns <- dawgDictionaryBuilderRefs ddb ! numOfUnusedUnits

  putStrLn $ concat [ "unfixed : ", show uix ]
  putStrLn $ concat [ "num_unused_states : ", show uns ]

-- ** Dictionary extra/blocks helpers

-- | Gets the entire block (hashtable) by its id. Throws an error if the block is missing.
lookupBlock
  :: (HasCallStack, PrimMonad m)
  => DictionaryBuilder_ m -> BaseType -> m (UUHT m BaseType DictionaryExtraUnit)
lookupBlock ddb ix =
  HT.lookup (dawgDictionaryBuilderExtras ddb) (ix `div` blockSize) >>= \case
    Nothing -> error "Missing block"
    Just block -> pure block
{-# INLINE lookupBlock #-}

-- | Inserts a block by its index into the hashtable.
insertBlock
  :: PrimMonad m
  => DictionaryBuilder_ m -> UUHT m BaseType DictionaryExtraUnit -> BaseType -> m ()
insertBlock ddb block ix =
  HT.insert (dawgDictionaryBuilderExtras ddb) (ix `div` blockSize) block
{-# INLINE insertBlock #-}

-- | Swap two blocks by their ids. Both blocks should be present.
-- If at least one of blocks is missing, error will be thrown.
swapBlocks
  :: (HasCallStack, PrimMonad m)
  => DictionaryBuilder_ m -> BaseType -> BaseType -> m ()
swapBlocks ddb b1 b2 = do
  let getBlock blockId =
        HT.lookup (dawgDictionaryBuilderExtras ddb) blockId >>= \case
          Nothing -> error "Missing block"
          Just block -> pure block

  block1 <- getBlock b1
  block2 <- getBlock b2

  HT.insert (dawgDictionaryBuilderExtras ddb) b1 block2
  HT.insert (dawgDictionaryBuilderExtras ddb) b2 block1
{-# INLINE swapBlocks #-}

-- | Replaces the content of the block by its id with empty units.
clearBlock
  :: HasCallStack => PrimMonad m => BaseType -> DictionaryBuilder_ m -> m ()
clearBlock !blockId ddb = do
  block <- HT.lookup (dawgDictionaryBuilderExtras ddb) blockId >>= \case
    Nothing -> error "Missing block"
    Just block -> pure block
  bsize <- HT.size block
  when (bsize > 0) do
    forM_ [0 .. pred bsize] \ix -> do
      HT.insert block (fromIntegral ix `mod` blockSize) Extra.empty
{-# INLINE clearBlock #-}

-- | Get block content by its id.
extras
  :: forall m. HasCallStack
  => PrimMonad m
  => DictionaryBuilder_ m -> BaseType -> m DictionaryExtraUnit
extras !ddb !ix = do
  !block <- lookupBlock ddb ix
  fromMaybe Extra.empty <$> HT.lookup block (ix `mod` blockSize)
{-# INLINE extras #-}

-- | Modifies block content by its id and modifier function.
modifyExtras
  :: HasCallStack
  => PrimMonad m
  => DictionaryBuilder_ m
  -> BaseType -> (DictionaryExtraUnit -> DictionaryExtraUnit) -> m ()
modifyExtras !ddb !ix modifier = do
  !block <- lookupBlock ddb ix
  let f Nothing = Just $! modifier Extra.empty
      f (Just !x) = Just $! modifier x
  HT.alter block f (ix `mod` blockSize)
{-# INLINE modifyExtras #-}

-- | Allocates new empty blocks by provided size, if it is greater than 'numOfBlocks'.
allocateExtras
  :: HasCallStack
  => PrimMonad m
  => DictionaryBuilder_ m -> BaseType -> m ()
allocateExtras !ddb destSize = do
  srcSize <- numOfBlocks ddb
  when (srcSize < destSize) do
    forM_ [srcSize .. pred destSize] \ix -> do
      block <- HT.initialize 0
      forM_ [0 .. pred blockSize] \bix -> do
        HT.insert block bix Extra.empty
      HT.insert (dawgDictionaryBuilderExtras ddb) ix block
{-# INLINE allocateExtras #-}

