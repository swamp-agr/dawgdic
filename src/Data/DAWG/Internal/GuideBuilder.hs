{-# LANGUAGE CPP #-}
module Data.DAWG.Internal.GuideBuilder where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
import Data.Primitive.MutVar
import GHC.Stack (HasCallStack)

import Data.Primitive.PrimArray.Combinators
import Data.DAWG.Internal.BaseType
import Data.DAWG.Internal.DAWG
import Data.DAWG.Internal.Dictionary
import Data.DAWG.Internal.Guide (Guide (..))
import Data.DAWG.Internal.GuideUnit (GuideUnit (..))

import qualified Data.Vector as Vector
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Unboxed as UV

import qualified Data.DAWG.Internal.DAWG as Dawg
import qualified Data.DAWG.Internal.Dictionary as Dict
import qualified Data.DAWG.Internal.GuideUnit as GuideUnit

#ifdef trace
import Data.Char
import Data.DAWG.Trace
#endif

-- ** Guide Builder

newtype GuideBuilder m = GRef { getGRef :: MutVar (PrimState m) (GuideBuilder_ m) }

data GuideBuilder_ m = GuideBuilder
  { guideBuilderDawg :: DAWG
  , guideBuilderDictionary :: Dictionary
  , guideBuilderUnits :: ObjectPool (PrimState m) GuideUnit
  , guideBuilderIsFixedTable :: ObjectPool (PrimState m) UCharType
  }

type GuideM m =
  ( PrimMonad m
  -- , MVector ObjectPool GuideUnit, MVector ObjectPool UCharType
  )

new :: GuideM m => DAWG -> Dictionary -> m (GuideBuilder m)
new guideBuilderDawg guideBuilderDictionary = do
  guideBuilderUnits <- V.new 0
  guideBuilderIsFixedTable <- V.new 0
  let g = GuideBuilder{..}
  GRef <$> newMutVar g
{-# INLINE new #-}

build :: HasCallStack => GuideM m => DAWG -> Dictionary -> m (Maybe Guide)
build dawg dict = do
  gref@GRef{..} <- new dawg dict
  resizeUnitsAndFlagsForGuide gref

  gb <- readMutVar getGRef
  if dictionarySize (guideBuilderDictionary gb) == 1
    then freeze gb
    else do
      buildGuideFromIndexes Dawg.root Dict.root gb >>= \case
        Nothing -> pure Nothing
        Just () -> freeze gb
{-# INLINE build #-}

build' :: HasCallStack => GuideM m => DAWG -> Dictionary -> m Guide
build' dawg dict = build dawg dict >>= \case
  Just guide -> pure guide
  Nothing -> error "failed to build guide"
{-# INLINE build' #-}

resizeUnitsAndFlagsForGuide :: GuideM m => GuideBuilder m -> m ()
resizeUnitsAndFlagsForGuide GRef{..} = do
  gb <- readMutVar getGRef
  let dictSize = fromIntegral $ dictionarySize $ guideBuilderDictionary gb
      unitsSize = V.length $ guideBuilderUnits gb
      flagsSize = V.length $ guideBuilderIsFixedTable gb
  newUnits <- V.grow (guideBuilderUnits gb)
    $ if unitsSize < dictSize then dictSize - unitsSize else 0
  newFlags <- V.grow (guideBuilderIsFixedTable gb)
    $ if flagsSize < dictSize then dictSize - flagsSize else 0
  let !ngb = gb { guideBuilderUnits = newUnits, guideBuilderIsFixedTable = newFlags }
  writeMutVar getGRef ngb
{-# INLINE resizeUnitsAndFlagsForGuide #-}

buildGuideFromIndexes :: GuideM m => BaseType -> BaseType -> GuideBuilder_ m -> m (Maybe ())
buildGuideFromIndexes !dawgIx !dictIx !gb = do
#ifdef trace
  ifd <- isFixed dictIx gb
  traceIO $ concat
    ["buildGuide dawgIx ", show dawgIx
    , " dawgChildIx ", show $ Dawg.child dawgIx (guideBuilderDawg gb)
    , " dictIx ", show dictIx, " is_fixed_dix ", show ifd
    ]
#endif
  isFixed dictIx gb >>= \case
    True -> pure (Just ())
    False -> do
      setIsFixed dictIx gb

      -- finds the first non-terminal child.
      let !dawg = guideBuilderDawg gb
          !dict = guideBuilderDictionary gb
          !dawgChildIx = Dawg.child dawgIx dawg
      dawgChildIxRef <- newMutVar dawgChildIx
#ifdef trace
      traceIO $ concat
        [ "-buildGuide dawgChildIx ", show dawgChildIx
        , " l ", show $ chr $ fromIntegral $ Dawg.label dawgChildIx dawg
        , " (", show $ Dawg.label dawgChildIx dawg, ")"
        ]
#endif
      when (Dawg.label dawgChildIx dawg == 0) do
        writeMutVar dawgChildIxRef $! Dawg.sibling dawgChildIx dawg

      !dawgChildIx' <- readMutVar dawgChildIxRef
      if dawgChildIx' == 0
        then pure $ Just ()
        else do
          guideBuilderUnits gb !<~~ dictIx
            $! GuideUnit.setChild $! Dawg.label dawgChildIx' dawg

          let go !ix !dictIx'
                | ix == 0 = pure $ Just ()
                | otherwise = do
                    let !childLabel = Dawg.label ix dawg
#ifdef trace
                    traceIO $ concat
                      ["-go dawgChildIx ", show ix, " dictChildIx ", show dictIx'
                      , " childLabel ", show childLabel
                      ]
#endif
                    case Dict.followChar (fromIntegral childLabel) dictIx' dict of
                      Nothing -> pure Nothing
                      Just dictChildIx -> do
#ifdef trace
                        traceIO $ concat
                          [ "-go follow dawgChildIx ", show ix
                          , " dictChildIx ", show dictChildIx
                          ]
#endif
                        -- outer recursion loop
                        buildGuideFromIndexes ix dictChildIx gb >>= \case
                          Nothing -> pure Nothing
                          Just () -> do
                            let !dawgSiblingIx = Dawg.sibling ix dawg
                                !siblingLabel = Dawg.label dawgSiblingIx dawg
                            when (dawgSiblingIx /= 0) do
                              guideBuilderUnits gb !<~~ dictChildIx
                                $! GuideUnit.setSibling siblingLabel

                            go dawgSiblingIx dictIx'

          go dawgChildIx' dictIx
{-# INLINE buildGuideFromIndexes #-}

setIsFixed :: GuideM m => BaseType -> GuideBuilder_ m -> m ()
setIsFixed !ix gb = do
  let setIsFixed' !v = v .|. (1 .<<. (fromIntegral ix `mod` 8))
  guideBuilderIsFixedTable gb !<~~ (fromIntegral ix `div` 8) $! setIsFixed'
{-# INLINE setIsFixed #-}

isFixed :: GuideM m => BaseType -> GuideBuilder_ m -> m Bool
isFixed !ix gb = do
  v <- guideBuilderIsFixedTable gb !~ (fromIntegral ix `div` 8)
  let x = v .&. (1 .<<. (fromIntegral ix `mod` 8))
  pure $ x /= 0
{-# INLINE isFixed #-}

freeze :: GuideM m => GuideBuilder_ m -> m (Maybe Guide)
freeze gb = do
  funits <- UV.freeze $ guideBuilderUnits gb  
  let !guideUnits = (Vector.fromList . UV.toList) funits
  let !guideSize = fromIntegral $ Vector.length guideUnits
  pure $ Just Guide{..}
{-# INLINE freeze #-}
