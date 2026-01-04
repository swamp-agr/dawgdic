{-# LANGUAGE CPP #-}
module Data.DAWG.Internal.RankedGuideBuilder where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
import Data.Primitive.MutVar
import Data.Vector.Algorithms.Insertion (sortByBounds)
import GHC.Stack (HasCallStack)

import Data.Primitive.PrimArray.Combinators
import Data.DAWG.Internal.BaseType
import Data.DAWG.Internal.GuideUnit
import Data.DAWG.Internal.Guide (Guide(..))
import Data.DAWG.Internal.DAWG (DAWG)
import Data.DAWG.Internal.Dictionary (Dictionary (..))
import Data.DAWG.Internal.RankedGuide
import Data.DAWG.Internal.RankedGuideLink

import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Unboxed as UV

import qualified Data.DAWG.Internal.DAWG as Dawg
import qualified Data.DAWG.Internal.Dictionary as Dict
import qualified Data.DAWG.Internal.DictionaryUnit as DictUnit
import qualified Data.DAWG.Internal.GuideUnit as GuideUnit
import qualified Data.DAWG.Internal.RankedGuideLink as Link

#ifdef trace
import Control.Monad (forM_)
import Data.Char (chr)
import Data.DAWG.Trace
#endif

-- ** Ranked Guide Builder

newtype RankedGuideBuilder m =
  RGBRef { getRGBRef :: MutVar (PrimState m) (RankedGuideBuilder_ m) }

data RankedGuideBuilder_ m = RankedGuideBuilder
  { rankedGuideBuilderDawg :: DAWG
  , rankedGuideBuilderDictionary :: Dictionary
  , rankedGuideBuilderUnits :: ObjectPool (PrimState m) GuideUnit
  , rankedGuideBuilderLinks :: ObjectPool (PrimState m) RankedGuideLink
  , rankedGuideBuilderIsFixedTable :: ObjectPool (PrimState m) UCharType
  }


build :: HasCallStack => PrimMonad m => DAWG -> Dictionary -> m (Maybe RankedGuide)
build dawg dict = do
  rgref@RGBRef{..} <- new dawg dict
  resizeUnitsAndFlags rgref

  rgb <- readMutVar getRGBRef
  if Dawg.size (rankedGuideBuilderDawg rgb) <= 1
    then freeze rgb >>= pure . Just
    else do
      maxValueRef <- newMutVar (-1)
      buildFromIndexes Dawg.root Dict.root maxValueRef rgref >>= \case
        False -> pure Nothing
        True -> readMutVar getRGBRef >>= freeze >>= pure . Just

-- | Same as 'build' but throws an error if build fails.
build' :: HasCallStack => PrimMonad m => DAWG -> Dictionary -> m RankedGuide
build' dawg dict = build dawg dict >>= \case
  Just guide -> pure guide
  Nothing -> error "failed to build guide"
{-# INLINE build' #-}

freeze :: PrimMonad m => RankedGuideBuilder_ m -> m RankedGuide
freeze rgb = do
  funits <- UV.freeze $ rankedGuideBuilderUnits rgb
  let !guideUnits = (UV.fromList . UV.toList) funits
      !guideSize = fromIntegral $ UV.length guideUnits
      !guideDictionary = rankedGuideBuilderDictionary rgb
      !rankedGuide = Guide{..}
  pure RankedGuide{..}

-- ** Helpers

new :: PrimMonad m => DAWG -> Dictionary -> m (RankedGuideBuilder m)
new rankedGuideBuilderDawg rankedGuideBuilderDictionary = do
  rankedGuideBuilderUnits <- V.new 0
  rankedGuideBuilderLinks <- V.new 0
  rankedGuideBuilderIsFixedTable <- V.new 0
  let rg = RankedGuideBuilder{..}
  RGBRef <$> newMutVar rg

resizeUnitsAndFlags :: PrimMonad m => RankedGuideBuilder m -> m ()
resizeUnitsAndFlags RGBRef{..} = do
  rgb <- readMutVar getRGBRef
  let dictSize = fromIntegral $ dictionarySize $ rankedGuideBuilderDictionary rgb
      unitsSize = V.length $ rankedGuideBuilderUnits rgb
      flagsSize = V.length $ rankedGuideBuilderIsFixedTable rgb
  newUnits <- V.grow (rankedGuideBuilderUnits rgb)
    $ if unitsSize < dictSize then dictSize - unitsSize else 0
  newFlags <- V.grow (rankedGuideBuilderIsFixedTable rgb)
    $ if flagsSize < (dictSize `div` 8) then (dictSize `div` 8) - flagsSize else 0
  let !nrgb = rgb
        { rankedGuideBuilderUnits = newUnits, rankedGuideBuilderIsFixedTable = newFlags }
  writeMutVar getRGBRef nrgb

buildFromIndexes
  :: PrimMonad m
  => BaseType -> BaseType -> MutVar (PrimState m) ValueType -> RankedGuideBuilder m -> m Bool
buildFromIndexes !dawgIx !dictIx !maxValueRef rgref@RGBRef{..} = do
#ifdef trace
  gb <- readMutVar getRGBRef
  ifd <- isFixed dictIx gb
  traceIO $ concat
    ["buildRankedGuide dawgIx ", show dawgIx
    , " dawgChildIx ", show $ Dawg.child dawgIx (rankedGuideBuilderDawg gb)
    , " dictIx ", show dictIx, " is_fixed_dix ", show ifd
    ]
#endif
  let sortLinks n rgb = do
        let links = rankedGuideBuilderLinks rgb
            l = V.length links
        sortByBounds compare links n l

      updateMaxValue n valueRef rgb = do
        link <- rankedGuideBuilderLinks rgb !~ fromIntegral n
        writeMutVar valueRef (Link.value link)
        
      resizeLinks n = do
        rgb <- readMutVar getRGBRef
        let links = rankedGuideBuilderLinks rgb
            l = V.length links
        newLinks <- case compare l n of
          LT -> V.grow links (n - l)
          EQ -> pure links
          GT -> pure (V.unsafeSlice 0 n links)
        let !nrgb = rgb { rankedGuideBuilderLinks = newLinks }
        writeMutVar getRGBRef nrgb
  rgb0 <- readMutVar getRGBRef
  isFixed dictIx rgb0 >>= \case
    True -> findMaxValue dictIx maxValueRef rgb0
    False -> do
      setIsFixed dictIx rgb0

      let initialNumLinks = V.length $ rankedGuideBuilderLinks rgb0
      enumerateLinks dawgIx dictIx rgref >>= \case
        False -> pure False
        True -> do
          -- enumerateLinks could resize vectors, thus, reading again
          rgb <- readMutVar getRGBRef
#ifdef trace
          traceIO $ concat
            [ "buildRankedGuide links-b ("
            , show initialNumLinks, ",0,"
            , show $ pred $ V.length $ rankedGuideBuilderLinks rgb
            , ")"
            ]
          dumpLinks rgref
#endif
          sortLinks initialNumLinks rgb
#ifdef trace
          traceIO "buildRankedGuide links-a"
          dumpLinks rgref
#endif
          turnLinksToUnits dictIx initialNumLinks rgref >>= \case
            False -> pure False
            True -> do
              updateMaxValue initialNumLinks maxValueRef rgb
              resizeLinks initialNumLinks
              pure True

findMaxValue
  :: PrimMonad m
  => BaseType -> MutVar (PrimState m) ValueType -> RankedGuideBuilder_ m -> m Bool
findMaxValue !startDictIx !maxValueRef rgb = do
  let follow' !dictIx = do
        childLabel <- GuideUnit.child <$> rankedGuideBuilderUnits rgb !~ fromIntegral dictIx
        if childLabel == 0
          then pure $! Just dictIx
          else case Dict.followChar (fromIntegral childLabel) dictIx (rankedGuideBuilderDictionary rgb) of
            Nothing -> pure Nothing
            Just lastDictIx -> follow' lastDictIx

  follow' startDictIx >>= \case
    Nothing -> pure False
    Just !dictIx -> do
      let itHasValue = Dict.hasValue dictIx (rankedGuideBuilderDictionary rgb)
      when itHasValue $ writeMutVar maxValueRef
        $! Dict.value dictIx (rankedGuideBuilderDictionary rgb)
      pure itHasValue            

enumerateLinks :: PrimMonad m => BaseType -> BaseType -> RankedGuideBuilder m -> m Bool
enumerateLinks !startDawgIx !dictIx rgref@RGBRef{..} = do
  rgb0 <- readMutVar getRGBRef
  let !startDawgChildIx = Dawg.child startDawgIx (rankedGuideBuilderDawg rgb0)
      go !dawgChildIx
        | dawgChildIx == 0 = pure True
        | otherwise = do
            rgb <- readMutVar getRGBRef
            let !startValue = -1
                !childLabel = Dawg.label dawgChildIx (rankedGuideBuilderDawg rgb)
            maxValueRef <- newMutVar startValue
#ifdef trace
            traceIO $ concat
              [ "enumerateLinks dawgChildIx ", show dawgChildIx
              , " dictIx ", show dictIx
              , " childLabel ", show $ chr $ fromIntegral childLabel
              ]
#endif
            mValue <- if childLabel == 0
              then if (not $ Dict.hasValue dictIx $ rankedGuideBuilderDictionary rgb)
                   then pure Nothing
                   else do
                     let !value' = Dict.value dictIx $ rankedGuideBuilderDictionary rgb
                     writeMutVar maxValueRef value'
                     pure $! Just value'
              else case Dict.followChar (fromIntegral childLabel) dictIx (rankedGuideBuilderDictionary rgb) of
                Nothing -> pure Nothing
                Just lastDictIx -> do
                   -- outer recursion loop
                  buildFromIndexes dawgChildIx lastDictIx maxValueRef rgref >>= \case
                    False -> pure Nothing
                    True -> do
                      value' <- readMutVar maxValueRef
                      pure $! Just value'
            case mValue of
              Nothing -> pure False
              Just value' -> do
#ifdef trace
                traceIO $ concat
                  [ "enumerateLinks dawgChildIx ", show dawgChildIx
                  , " dictIx ", show dictIx
                  , " link ", show (RankedGuideLink (childLabel, value'))
                  ]
#endif
                pushLinkBack (RankedGuideLink (childLabel, value'))
                rgb1 <- readMutVar getRGBRef
                let nextDawgIx = Dawg.sibling dawgChildIx (rankedGuideBuilderDawg rgb1)
                go nextDawgIx

      pushLinkBack !link = do
        rgb <- readMutVar getRGBRef
        let linksSize = V.length $ rankedGuideBuilderLinks rgb
        newLinks <- V.grow (rankedGuideBuilderLinks rgb) 1
        newLinks !<~~ fromIntegral linksSize $ const link
        let !nrgb = rgb { rankedGuideBuilderLinks = newLinks }
        writeMutVar getRGBRef nrgb

  go startDawgChildIx

turnLinksToUnits :: PrimMonad m => BaseType -> Int -> RankedGuideBuilder m -> m Bool
#ifdef trace
turnLinksToUnits !dictIx !linksBegin rgref@RGBRef{..} = do
#else
turnLinksToUnits !dictIx !linksBegin RGBRef{..} = do
#endif
  rgb <- readMutVar getRGBRef
#ifdef trace
  link <- rankedGuideBuilderLinks rgb !~ fromIntegral linksBegin
  traceIO $ concat
    ["turnLinksToUnits dictIx ", show dictIx, " links-b ", show linksBegin
    , " link ", show link
    ]
  dumpLinks rgref
#endif
  let !lsize = V.length $ rankedGuideBuilderLinks rgb
      links = rankedGuideBuilderLinks rgb
      units = rankedGuideBuilderUnits rgb
  -- the first child
  !firstLabel <- Link.label <$> links !~ fromIntegral linksBegin
  units !<~~ fromIntegral dictIx $ GuideUnit.setChild firstLabel
  !dictStartChildIx <- followWithoutCheck dictIx firstLabel rgb

  -- other children
  let go !dictChildIx !ix
        | ix >= lsize = pure True
        | otherwise = do
            !siblingLabel <- Link.label <$> links !~ fromIntegral ix
            !dictSiblingIx <- followWithoutCheck dictIx siblingLabel rgb
            units !<~~ fromIntegral dictChildIx $ GuideUnit.setSibling siblingLabel
            go dictSiblingIx (succ ix)
  go dictStartChildIx (succ linksBegin)

followWithoutCheck
  :: PrimMonad m => BaseType -> UCharType -> RankedGuideBuilder_ m -> m BaseType
followWithoutCheck !ix !label' rgb = do
  let dictUnits = Dict.dictionaryUnits $ rankedGuideBuilderDictionary rgb
      offset' = DictUnit.offset $ (dictUnits UV.! fromIntegral ix)
  pure ((ix .^. offset') .^. fromIntegral label')

setIsFixed :: PrimMonad m => BaseType -> RankedGuideBuilder_ m -> m ()
setIsFixed !ix rgb = do
  let setIsFixed' !v = v .|. (1 .<<. (fromIntegral ix `mod` 8))
  rankedGuideBuilderIsFixedTable rgb !<~~ (fromIntegral ix `div` 8) $! setIsFixed'
                             
isFixed :: PrimMonad m => BaseType -> RankedGuideBuilder_ m -> m Bool
isFixed !ix rgb = do
  v <- rankedGuideBuilderIsFixedTable rgb !~ (fromIntegral ix `div` 8)
  let x = v .&. (1 .<<. (fromIntegral ix `mod` 8))
  pure $! x /= 0

#ifdef trace
dumpLinks :: PrimMonad m => RankedGuideBuilder m -> m ()
dumpLinks RGBRef{..} = do
  rgb <- readMutVar getRGBRef
  let lsize = V.length $ rankedGuideBuilderLinks rgb
  traceIO $ concat [ "ix(", show lsize, ")\tlabel\tvalue" ]
  forM_ [0 .. (pred lsize)] \ix -> do
    link <- rankedGuideBuilderLinks rgb !~ fromIntegral ix
    traceIO $ concat [ show ix, "\t", show link ]
#endif
