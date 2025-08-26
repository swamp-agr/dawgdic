{-|
Module: Data.DAWG.Internal.LinkTable
Description: Exports link table used to build dictionary as well as its internal API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
{-# LANGUAGE BangPatterns #-}
module Data.DAWG.Internal.LinkTable where

import Control.Monad (forM_, when)
import Control.Monad.Primitive (PrimMonad)
import Data.Maybe (fromMaybe)

import Data.DAWG.Internal.BaseType

import qualified Data.Vector.Hashtables as HT

-- ** 'LinkTable'

-- | Alias for 'UUHT' that holds a pair of index with offset as value
-- associated with a hash as a key.
type LinkTable m = UUHT m BaseType (BaseType, BaseType)

-- | Allocates a table space with given size.
init :: PrimMonad m => LinkTable m -> BaseType -> m ()
init ht size = do
  when (size > 0) $ forM_ [0 .. size - 1] \ix -> do
    HT.insert ht (fromIntegral ix) (0, 0)
{-# INLINE init #-}

-- | Stores index with offset into the table.
insert :: PrimMonad m => LinkTable m -> BaseType -> BaseType -> m ()
insert ht !ix !offset = do
  !hid <- findId ht ix
  HT.insert ht hid (ix, offset)
{-# INLINE insert #-}

-- | Find an offset that corresponds to a given index.
find :: PrimMonad m => LinkTable m -> BaseType -> m BaseType
find ht ix = do
  !hid <- findId ht ix
  HT.lookup ht hid >>= pure . fromMaybe 0 . fmap snd
{-# INLINE find #-}

-- ** Helpers

-- | Finds a hash associated with a given index.
findId :: PrimMonad m => LinkTable m -> BaseType -> m BaseType
findId ht !ix = do
  htsize <- HT.size ht
  let !startHid = fromIntegral (hashBaseType ix `mod` htsize)
      go !hid = do
        let !nextHid = succ hid `mod` fromIntegral htsize
        HT.lookup ht (fromIntegral hid) >>= \case
          Nothing -> pure hid
          Just result -> if fst result == ix || fst result == 0
            then pure hid
            else go nextHid
  go startHid
{-# INLINE findId #-}

