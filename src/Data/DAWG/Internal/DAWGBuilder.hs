{-# LANGUAGE CPP #-}
module Data.DAWG.Internal.DAWGBuilder where

import Control.Monad (forM_, when, unless)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bit (Bit (..))
import Data.Bits
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Primitive.MutVar
import Data.Vector (Vector)
import GHC.Stack (HasCallStack)
import Prelude hiding (init)

import Data.Primitive.PrimArray.Combinators
import Data.DAWG.Internal.BaseType
import Data.DAWG.Internal.BaseUnit (BaseUnit (..), base, hasSibling, isState)
import Data.DAWG.Internal.DAWG
  (DAWG (..), BasePool (..), BitPool (..), FlagPool (..), LabelPool (..))
import Data.DAWG.Internal.DAWGUnit (DAWGUnit (..))
import Data.DAWG.Internal.Stack

import qualified Data.DAWG.Internal.BaseUnit as BaseUnit
import qualified Data.DAWG.Internal.DAWGUnit as DawgUnit

import qualified Data.Primitive.PrimArray.Utils as A
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Hashtables as HT

#ifdef trace
import Data.DAWG.Trace
#endif

-- ** DAWG Builder

newtype DAWGBuilder m = DBRef { unDBRef :: MutVar (PrimState m) (DAWGBuilder_ m) }

data DAWGBuilder_ m = DAWGBuilder
  { dawgBuilderBasePool :: ObjectPool (PrimState m) BaseUnit
  , dawgBuilderLabelPool :: ObjectPool (PrimState m) UCharType
  , dawgBuilderFlagPool :: ObjectPool (PrimState m) Bit
  , dawgBuilderUnitPool :: ObjectPool (PrimState m) DAWGUnit
  , dawgBuilderHashTable :: UUHT m BaseType BaseType
  , dawgBuilderUnfixedUnits :: Stack (PrimState m) BaseType
  , dawgBuilderUnusedUnits :: Stack (PrimState m) BaseType
  , dawgBuilderRefs :: !(IntArray (PrimState m))
  }

type DawgBuilderM m =
  ( PrimMonad m
  -- , MVector ObjectPool DAWGUnit
  )

numOfStates, numOfMergedTransitions, numOfMergingStates :: Int
numOfStates = 0
{-# INLINE numOfStates #-}
numOfMergedTransitions = 1
{-# INLINE numOfMergedTransitions #-}
numOfMergingStates = 2
{-# INLINE numOfMergingStates #-}

new :: DawgBuilderM m => m (DAWGBuilder m)
new = do
  dawgBuilderHashTable <- HT.initialize 0
  dawgBuilderBasePool <- V.new 0 
  dawgBuilderLabelPool <- V.new 0
  dawgBuilderFlagPool <- V.new 0
  dawgBuilderUnitPool <- V.new 0
  dawgBuilderUnfixedUnits <- StackRef <$> newMutVar EndOfStack
  dawgBuilderUnusedUnits <- StackRef <$> newMutVar EndOfStack
  dawgBuilderRefs <- A.replicate 3 0
  dawgBuilderRefs <~ numOfStates $ 1
  let db = DAWGBuilder{..}
  unDBRef <- newMutVar db
  pure DBRef {..}  
{-# INLINE new #-}

-- | Initializes an object.
init
  :: DawgBuilderM m
  => DAWGBuilder m -> m ()
init dbref = do
  do
    !db <- readMutVar (unDBRef dbref)
    let initHtSize = 1 .<<. 8
    resize (dawgBuilderHashTable db) initHtSize

  !_ <- allocateUnit dbref
  !_ <- allocateTransition dbref
  db <- readMutVar (unDBRef dbref)
  dawgBuilderUnitPool db <~~ 0 $ DawgUnit.setLabel DawgUnit.empty 0xFF
  push 0 (dawgBuilderUnfixedUnits db)
{-# INLINE init #-}

insert
  :: HasCallStack
  => DawgBuilderM m
  => Vector Char
  -> Maybe ValueType
  -> DAWGBuilder m
  -> m Bool
insert ks mValue db = do
  let v = fromMaybe 0 mValue
  if not (Vector.null ks || ks == Vector.singleton '\0' || v < 0)
    then do
      let ks' = Vector.takeWhile (/= '\0') ks
          l = Vector.length ks'
      insertKey ks' l v db
    else pure False
{-# INLINE insert #-}

insertWithLength
  :: HasCallStack
  => DawgBuilderM m
  => Vector Char
  -> Int
  -> ValueType
  -> DAWGBuilder m
  -> m Bool
insertWithLength ks l v db =
  if not (Vector.null ks || ks == Vector.singleton '\0' || v < 0)
    then if (Vector.notElem '\0' ks)
      then insertKey ks l v db
      else pure False
    else pure False
{-# INLINE insertWithLength #-}

insertKey
  :: HasCallStack
  => DawgBuilderM m
  => Vector Char -- ^ entire word as vector of keys
  -> Int -- ^ Length of key
  -> ValueType -- ^ Value
  -> DAWGBuilder m
  -> m Bool
insertKey ks l v dbref@DBRef{..} = do
  do
    db <- readMutVar unDBRef
    htsize <- HT.size $ dawgBuilderHashTable db
    when (htsize == 0) $ init dbref

  -- Find separate unit
  let findSeparateUnit (!ix, !keyPos)
        | keyPos > fromIntegral l = pure $ Just (ix, keyPos)
        | otherwise = do
            db <- readMutVar unDBRef
            !u0 <- dawgBuilderUnitPool db !~ ix
            let !childIx = DawgUnit.child u0
            if childIx == 0
              then pure $ Just (ix, keyPos)
              else do
                let !keyLabel = if keyPos < fromIntegral l then ks Vector.! keyPos else '\0'
                !u' <- dawgBuilderUnitPool db !~ childIx
                let !unitLabel = DawgUnit.label u'
#ifdef trace
                traceIO $ concat
                  [ "findSeparateUnit ix ", show ix
                  , " keyPos ", show keyPos
                  , " keyLabel ", show keyLabel
                  , " (", show $ ord keyLabel
                  , ") unitLabel ",show $ chr $ fromIntegral unitLabel
                  , " (", show unitLabel, ")"
                  ]
#endif
                if ord keyLabel < fromIntegral unitLabel
                  then pure Nothing
                  else if ord keyLabel > fromIntegral unitLabel
                    then do
                      dawgBuilderUnitPool db <~~ childIx $ DawgUnit.setHasSibling u' True
                      fixUnits childIx dbref
                      pure $ Just (ix, keyPos)
                    else findSeparateUnit (childIx, succ keyPos)

      addNewUnit (!ix, !keyPos)
        | keyPos > fromIntegral l = pure (ix, keyPos)
        | otherwise = do
            let keyLabel = if keyPos < fromIntegral l then ks Vector.! keyPos else '\0'
            childIx <- allocateUnit dbref
            ndb <- readMutVar unDBRef
            !u <- dawgBuilderUnitPool ndb !~ ix
            let setState !u' = if DawgUnit.child u == 0
                  then DawgUnit.setIsState u' True
                  else u'
                !nu = setState
                  $! flip DawgUnit.setSibling (DawgUnit.child u)
                  $! flip DawgUnit.setLabel (fromIntegral $! ord keyLabel)
                  $! DawgUnit.empty

            dawgBuilderUnitPool ndb <~~ childIx $ nu
            dawgBuilderUnitPool ndb <~~ ix $ DawgUnit.setChild u childIx
            push childIx (dawgBuilderUnfixedUnits ndb)

            addNewUnit (childIx, succ keyPos)
        
  findSeparateUnit (0, 0) >>= \case
    Nothing -> pure False
    Just (!ix, !keyPos) -> do
      (!lastIx, _) <- addNewUnit (ix, keyPos)
      ndb <- readMutVar unDBRef
      lu <- dawgBuilderUnitPool ndb !~ lastIx
      dawgBuilderUnitPool ndb <~~ lastIx $ DawgUnit.setChild lu (fromIntegral v)

#ifdef trace
      do
        traceWith dump dbref
#endif
      pure True

freeze
  :: HasCallStack
  => DawgBuilderM m => DAWGBuilder m -> m DAWG
freeze dbref@DBRef{..} = do
  do
    db0 <- readMutVar unDBRef
    htsize <- HT.size $ dawgBuilderHashTable db0
    when (htsize == 0) $ init dbref
#ifdef trace
    traceIO "freeze"
#endif
    fixUnits 0 dbref

  db <- readMutVar unDBRef
  unit0 <- dawgBuilderUnitPool db !~ 0
#ifdef trace
  traceIO $ "freeze: u0 " <> show unit0
#endif
  dawgBuilderBasePool db <~~ 0 $ BaseUnit $ DawgUnit.base unit0
  dawgBuilderLabelPool db <~~ 0 $ DawgUnit.label unit0

#ifdef trace
  b0 <- dawgBuilderBasePool db !~ 0
  traceIO $ "freeze: b0 " <> show b0
#endif

  fbasePool <- VG.unsafeFreeze $ dawgBuilderBasePool db
  flabelPool <- VG.unsafeFreeze $ dawgBuilderLabelPool db
  fflagPool <- VG.unsafeFreeze $ dawgBuilderFlagPool db
  fnumOfStates <- dawgBuilderRefs db ! numOfStates
  fnumOfMergedTransitions <- dawgBuilderRefs db ! numOfMergedTransitions
  fnumOfMergingStates <- dawgBuilderRefs db ! numOfMergingStates
  
  let numOfTransitions = VG.length fbasePool - 1
      numOfMergedStates = numOfTransitions + fnumOfMergedTransitions + 1 - fnumOfStates

  pure DAWG
    { dawgBasePool = BasePool fbasePool
    , dawgLabelPool = LabelPool flabelPool
    , dawgFlagPool = FlagPool $ BitPool fflagPool
    , dawgNumOfStates = fromIntegral fnumOfStates
    , dawgNumOfMergedTransitions = fromIntegral fnumOfMergedTransitions
    , dawgNumOfMergedStates = fromIntegral numOfMergedStates
    , dawgNumOfMergingStates = fromIntegral fnumOfMergingStates
    }
{-# INLINE freeze #-}

fromAscList :: HasCallStack => PrimMonad m => [String] -> m DAWG
fromAscList lexicon = do
  db <- new
  forM_ lexicon \w -> do
    insert (Vector.fromList w) Nothing db
  freeze db
{-# INLINE fromAscList #-}

-- | Gets a unit from an object pool.
allocateUnit
  :: HasCallStack
  => DawgBuilderM m
  => DAWGBuilder m -> m BaseType
allocateUnit DBRef{..} = do
  !db <- readMutVar unDBRef
  (!index, !ndb) <- readMutVar (unStackRef $ dawgBuilderUnusedUnits db) >>= \case
    -- no unused units left
    EndOfStack -> do
      newUnitPool <- V.grow (dawgBuilderUnitPool db) 1
      let !index = pred $ V.length newUnitPool
          !nextDb = db { dawgBuilderUnitPool = newUnitPool }
      dawgBuilderUnitPool nextDb <~~ fromIntegral index $ DawgUnit.empty
      pure (fromIntegral index, nextDb)
    Elem !index !stack -> do
      writeMutVar (unStackRef $ dawgBuilderUnusedUnits db) stack
      pure (index, db)
  writeMutVar unDBRef ndb
  pure index

freeUnit :: HasCallStack => DawgBuilderM m => DAWGBuilder_ m -> BaseType -> m ()
freeUnit db !ix = do
  prevStack <- readMutVar (unStackRef $ dawgBuilderUnusedUnits db)
  let !stack = Elem ix prevStack
  writeMutVar (unStackRef $ dawgBuilderUnusedUnits db) stack
{-# INLINE freeUnit #-}

-- | Gets a transition from object pools.
allocateTransition
  :: HasCallStack
  => PrimMonad m => DAWGBuilder m -> m BaseType
allocateTransition DBRef{..} = do
  db <- readMutVar unDBRef
  newFlagPool <- V.grow (dawgBuilderFlagPool db) 1
  newFlagPool <~~ fromIntegral (V.length newFlagPool - 1) $ 0

  newBasePool <- V.grow (dawgBuilderBasePool db) 1
  newBasePool <~~ fromIntegral (V.length newBasePool - 1) $ BaseUnit.empty

  newLabelPool <- V.grow (dawgBuilderLabelPool db) 1
  newLabelPool <~~ fromIntegral (V.length newLabelPool - 1) $ 0

  let !lastIx = V.length newLabelPool - 1
      !ndb = db
        { dawgBuilderFlagPool = newFlagPool
        , dawgBuilderBasePool = newBasePool
        , dawgBuilderLabelPool = newLabelPool
        }
  writeMutVar unDBRef ndb
  pure (fromIntegral lastIx)

fixUnits
  :: HasCallStack
  => DawgBuilderM m
  => BaseType -> DAWGBuilder m -> m ()
fixUnits !index dbref@DBRef{..} = do
  let countSiblings !acc 0 = pure acc
      countSiblings !(acc :: Int) !ix = do
        db <- readMutVar unDBRef
        unit' <- dawgBuilderUnitPool db !~ ix
        countSiblings (succ acc) (DawgUnit.sibling unit')

      -- this operation potentially mutates dawg builder:
      -- re-read its content after its usage
      getTransitionIndex !siblings !ix !tix = if ix < siblings
        then do
          !nextTix <- allocateTransition dbref
          getTransitionIndex siblings (succ ix) nextTix
        else pure tix

      goBaseLabel (0 :: BaseType) !tix = pure tix
      goBaseLabel !ix !tix = do
        db <- readMutVar unDBRef
        !unit' <- dawgBuilderUnitPool db !~ ix
        let !nextIx = DawgUnit.sibling unit'

        dawgBuilderBasePool db <~~ tix $ (BaseUnit $ DawgUnit.base unit')
        dawgBuilderLabelPool db <~~ tix $ DawgUnit.label unit'
        goBaseLabel nextIx (pred tix)

      deleteFixedUnits !_ 0 = pure ()
      deleteFixedUnits !db !current = do
          !unit' <- dawgBuilderUnitPool db !~ current
          let !next = DawgUnit.sibling unit'
#ifdef trace
          traceIO $ concat
            [ "-deleteFixedUnit cur ", show current
            , " next ", show next]
#endif
          freeUnit db current
          deleteFixedUnits db next

      goStack = do
        db <- readMutVar unDBRef
        top (dawgBuilderUnfixedUnits db) >>= \case
          Nothing -> pure ()
          Just !unfixedIx -> do
            unless (unfixedIx == index) do
#ifdef trace
              traceIO ("goStack ix " <> show index <> " uix " <> show unfixedIx)
              traceWith dump dbref
#endif
              pop (dawgBuilderUnfixedUnits db)

              numOfStates' <- dawgBuilderRefs db ! numOfStates
              htsize <- HT.size $ dawgBuilderHashTable db
              when (numOfStates' >= htsize - (htsize .>>. 1)) do
                expandHashTable dbref

              numOfSiblings <- countSiblings 0 unfixedIx
              (hashId, matchedIx) <- findUnit unfixedIx dbref
#ifdef trace
              traceIO $ concat
                [ "goStack unfixedIx ", show unfixedIx
                , " hashId ", show hashId
                , " matchedIx ", show matchedIx
                ]
#endif

              nextMatchedIx <- if matchedIx /= 0
                then do
                  prevNumOfMergedTransitions <- dawgBuilderRefs db ! numOfMergedTransitions
                  let !nextNumOfMergedTransitions = prevNumOfMergedTransitions + numOfSiblings
                  dawgBuilderRefs db <~ numOfMergedTransitions $ nextNumOfMergedTransitions
                  flag' <- dawgBuilderFlagPool db !~ matchedIx

                  -- Records a merging state.
                  when (flag' == 0) do
                    prevMergingStates <- dawgBuilderRefs db ! numOfMergingStates 
                    dawgBuilderRefs db <~ numOfMergingStates $ prevMergingStates + 1
                    dawgBuilderFlagPool db <~~ matchedIx $ 1
                  pure matchedIx
                else do
                  !startTransitionIndex <- getTransitionIndex numOfSiblings 0 0
#ifdef trace
                  traceIO $ concat
                    [ "goStack matchedIx 0 nos ", show numOfSiblings
                    , " start tix ", show startTransitionIndex ]
#endif
                  -- re-read from mutable variable
                  ndb <- readMutVar unDBRef
                  !transitionIndex <- goBaseLabel unfixedIx startTransitionIndex
#ifdef trace
                  traceIO $ concat
                    [ "goStack matchedIx 0 nos ", show numOfSiblings
                    , " end tix ", show transitionIndex ]
#endif
                  let newMatchedIx = succ transitionIndex
                  HT.insert (dawgBuilderHashTable ndb) hashId newMatchedIx
                  prevStates <- dawgBuilderRefs ndb ! numOfStates
                  dawgBuilderRefs ndb <~ numOfStates $ prevStates + 1
                  pure newMatchedIx

              -- Delete fixed units
              ndb <- readMutVar unDBRef
              deleteFixedUnits ndb unfixedIx

              top (dawgBuilderUnfixedUnits ndb) >>= \case
                Nothing -> pure ()
                Just !nextUnfixedIx -> do
#ifdef trace
                  traceIO $ concat
                    [ "goStack setChild unfixedIx ", show nextUnfixedIx
                    , " matchedIx ", show nextMatchedIx
                    ]
#endif

                  dawgBuilderUnitPool ndb !<~~ nextUnfixedIx
                    $! flip DawgUnit.setChild nextMatchedIx

              writeMutVar unDBRef ndb
              goStack

#ifdef trace
  traceIO ("fixUnits ix " <> show index)
#endif
  goStack
  readMutVar unDBRef >>= \ldb -> do
    pop (dawgBuilderUnfixedUnits ldb)
    writeMutVar unDBRef ldb

expandHashTable
  :: DawgBuilderM m
  => DAWGBuilder m -> m ()
expandHashTable dbref@DBRef{..} = do
  db <- readMutVar unDBRef
  htsize <- HT.size $ dawgBuilderHashTable db
  let newSize = htsize .<<. 1
  resize (dawgBuilderHashTable db) newSize

  let go !ix !base'
        | ix == 0 = pure ()
        | otherwise = do
          label' <- dawgBuilderLabelPool db !~ fromIntegral ix
          when (label' == 0 || isState base') do
            let !bix = fromIntegral ix
            !hashId <- findTransition bix dbref
            HT.insert (dawgBuilderHashTable db) hashId bix

  V.iforM_ (dawgBuilderBasePool db) go
#ifdef trace
  traceIO $ "expandHashTable done"
#endif
{-# INLINE expandHashTable #-}

findTransition
  :: DawgBuilderM m
  => BaseType -> DAWGBuilder m -> m BaseType
findTransition !index dbref@DBRef{..} = do
  db <- readMutVar unDBRef
  !htsize <- HT.size $ dawgBuilderHashTable db
  !unit' <- hashTransition index dbref
  let !startHashId = unit' `mod` fromIntegral htsize

      go !hid = do
        mTransitionId <- HT.lookup (dawgBuilderHashTable db) hid
        let transitionId = fromMaybe 0 mTransitionId
#ifdef trace
        traceIO $ concat ["-findTransition ix ", show index, " hid ", show hid, " tid ", show transitionId]
#endif
        if transitionId == 0
          then pure hid
          else do
            !htsize' <- HT.size $ dawgBuilderHashTable db
            go (succ hid `mod` fromIntegral htsize')

  go startHashId
{-# INLINE findTransition #-}

findUnit
  :: DawgBuilderM m
  => BaseType -> DAWGBuilder m -> m (BaseType, BaseType)
findUnit !unitIndex dbref@DBRef{..} = do
  db <- readMutVar unDBRef
  !htsize0 <- HT.size $ dawgBuilderHashTable db
  !unit' <- hashUnit unitIndex dbref
  let !hashId = unit' `mod` fromIntegral htsize0

      findInTable !hid = do
        htsize <- HT.size $ dawgBuilderHashTable db
        mTransitionId <- HT.lookup (dawgBuilderHashTable db) hid
        let transitionId = fromMaybe 0 mTransitionId
#ifdef trace
        traceIO $ concat
          ["-findUnit uix ", show unitIndex
          , " hid ", show hid
          , " tix ", show transitionId
          ]
#endif
        if transitionId == 0
          then pure (hid, 0)
          else areEqual unitIndex transitionId dbref >>= \case
            True -> do
#ifdef trace
              traceIO $ concat
                ["--areEqual uix ", show unitIndex, " tix ", show transitionId]
#endif
              pure (hid, transitionId)
            False -> findInTable (succ hid `mod` fromIntegral htsize)
#ifdef trace
  traceIO $ concat
    ["-findUnit uix ", show unitIndex
    , " start hid ", show hashId
    , " ht.size ", show htsize0
    ]
#endif
  findInTable hashId
{-# INLINE findUnit #-}

areEqual
  :: DawgBuilderM m
  => BaseType -> BaseType -> DAWGBuilder m -> m Bool
areEqual !unitIndex !transitionIndex !DBRef{..} = do
  db <- readMutVar unDBRef
  !startUnit <- dawgBuilderUnitPool db !~ unitIndex
  let !startIx = DawgUnit.sibling startUnit
#ifdef trace
  traceIO $ concat
    [ "--areEqual start ", show startIx, " tix ", show transitionIndex]
#endif

  -- Mismatch: at this point there should be no siblings in associated base pool
  let goUnit !tix 0 = do
#ifdef trace
        base' <- dawgBuilderBasePool db !~ tix
        traceIO $ concat
          [ "--areEqual 0 tix ", show tix
          , " b ", show (base base')
          , " b.hs ", show (hasSibling base')
          ]
#endif
        pure (tix, False)
      goUnit !tix !uix = do
#ifdef trace
        traceIO $ concat [ "goUnit tix ", show tix, " uix ", show uix]
#endif
        base' <- dawgBuilderBasePool db !~ tix
        let !baseHasSibling = hasSibling base'
        if not baseHasSibling
          then pure (tix, True)
          else do
            !unit <- dawgBuilderUnitPool db !~ uix
            goUnit (succ tix) (DawgUnit.sibling unit)

      goBack !tix 0 = pure (tix, True)
      goBack !tix !(uix :: BaseType) = do
        !unit' <- dawgBuilderUnitPool db !~ uix
        !base' <- dawgBuilderBasePool db !~ tix
        !label' <- dawgBuilderLabelPool db !~ tix
#ifdef trace
        traceIO $ concat
          [ "goBack tix ", show tix
          , " uix ", show uix
          , " u ", show unit'
          , " tr ", show (base base')
          ," l ", show (chr $ fromIntegral label'), " (", show label', ")"
          ]
#endif
        if DawgUnit.base unit' /= base base' || DawgUnit.label unit' /= label'
           then pure (tix, False)
           else goBack (pred tix) (fromIntegral $ DawgUnit.sibling unit')

  (outTransitionIndex, inTransitionMismatch) <-
    goUnit (fromIntegral transitionIndex) (fromIntegral startIx)

  if inTransitionMismatch
    then pure False
    else snd <$> goBack outTransitionIndex (fromIntegral unitIndex)
{-# INLINE areEqual #-}

-- | Calculates a hash value from a transition.
hashTransition :: forall m. PrimMonad m => BaseType -> DAWGBuilder m -> m BaseType
hashTransition !ix DBRef{..} = do
  db <- readMutVar unDBRef
  let go !hv (0 :: BaseType) = pure hv
      go !hv !ix' = do
        !bu <- dawgBuilderBasePool db !~ ix'
#ifdef trace
        traceIO (concat [ "--hashTransition ix ", show ix', " hv ", show hv, " b ", show bu])
#endif
        let !itHasSibling = hasSibling bu
            !base' = base bu
        !label' <- fromIntegral @_ @BaseType <$> dawgBuilderLabelPool db !~ ix'
        let !newHashValue = hv .^. hashBaseType ((label' .<<. 24) .^. fromIntegral base')
        if itHasSibling then go newHashValue (succ ix') else pure newHashValue
  fromIntegral <$> go 0 ix
{-# INLINE hashTransition #-}

-- | Calculates a hash value from a unit.
hashUnit
  :: forall m. DawgBuilderM m
  => BaseType -> DAWGBuilder m -> m BaseType
hashUnit !ix DBRef{..} = do
  db <- readMutVar unDBRef

  let go :: BaseType -> BaseType -> m BaseType
      go !hv 0 = pure hv
      go !hv !ix' = do
        !u <- dawgBuilderUnitPool db !~ ix'
#ifdef trace
        traceIO (concat [ "--hashUnit ix ", show ix', " hv ", show hv, " u ", show u])
#endif
        let !base' = DawgUnit.base u
            !label' = DawgUnit.label u
            !newHashValue = hv .^. fromIntegral
              (hashBaseType ((fromIntegral label' .<<. 24) .^. fromIntegral base'))
            !next = fromIntegral $! DawgUnit.sibling u
        go newHashValue next

  go 0 (fromIntegral ix)
{-# INLINE hashUnit #-}

dump :: DAWGBuilder IO -> IO ()
dump DBRef{..} = do
  db <- readMutVar unDBRef
  
  let !bs = V.length $ dawgBuilderBasePool db
      !ls = V.length $ dawgBuilderLabelPool db
      !us = V.length $ dawgBuilderUnitPool db
      !ms = maximum [bs, ls, us]

  putStrLn $ concat [ "b(", show bs, ")\tl(", show ls, ")\tu(", show us, ")" ]

  forM_ [0 .. ms - 1] \i -> do
    !b <- fromMaybe (BaseUnit 0) <$> (V.readMaybe (dawgBuilderBasePool db) i)
    !l <- fromMaybe 0 <$> (V.readMaybe (dawgBuilderLabelPool db) i)
    !u <- fromMaybe DawgUnit.empty <$> (V.readMaybe (dawgBuilderUnitPool db) i)

    putStrLn $ concat
      [ show b, "\t", show (chr $ fromIntegral l), " ", show l, "\t", show u
      ]

  let topStr stack = readMutVar (unStackRef $ stack db) >>= \case
        EndOfStack -> pure ""
        Elem el _ -> pure $ show el

  unfixed <- topStr dawgBuilderUnfixedUnits
  unused <- topStr dawgBuilderUnusedUnits
  !htsize <- HT.size $ dawgBuilderHashTable db

  putStrLn $ "unfixed : " <> unfixed
  putStrLn $ "unused : " <> unused
  putStrLn $ "ht.size : " <> show htsize

-- ** HashTable helper

resize :: PrimMonad m => UUHT m BaseType BaseType -> Int -> m ()
resize ht newSize = do
  !htsize <- HT.size ht
#ifdef trace
  traceIO $ concat ["--resize old ", show htsize, " new ", show newSize]
#endif
  case compare htsize newSize of
    LT -> do
      forM_ [htsize .. newSize - 1] \ix -> do
        HT.insert ht (fromIntegral ix) 0
    EQ -> pure ()
    GT -> do
      forM_ [newSize .. htsize - 1] \ix -> do
        HT.delete ht (fromIntegral ix)
{-# INLINE resize #-}
