{-|
Module: Data.DAWG.Internal.Completer
Description: Exports completer as well as its internal API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
{-# LANGUAGE CPP #-}
module Data.DAWG.Internal.Completer where

import Control.Monad.ST (runST)
import Data.Char
import Data.Vector.Unboxed (Vector)
import GHC.Stack (HasCallStack)

import Data.DAWG.Internal.BaseType
import Data.DAWG.Internal.Dictionary (Dictionary (..))
import Data.DAWG.Internal.Guide (Guide (..))
import Data.DAWG.Internal.Stack

import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as VM

import qualified Data.DAWG.Internal.Dictionary as Dict
import qualified Data.DAWG.Internal.Guide as Guide

#ifdef trace
import System.IO.Unsafe
import Data.DAWG.Trace
#endif

-- ** Completer

-- | Use 'Completer' to perform completion requests by given word prefixes. It accumulates data during traversing dictionary via associated guide. Resulted completion could be accessed via 'keyToString' helper.
data Completer = Completer
  { completerDictionary :: !Dictionary
  , completerGuide :: !Guide
  , completerKey :: !(Vector UCharType)
  , completerIndexStack :: !Stack_
  , completerLastIndex :: !BaseType
  }

-- | Starts completion process for 'Completer' with a 'Dictionary' index and word prefix. For basic usage pass @0@ (dictionary 'Data.DAWG.Internal.Dictionary.root' index) as index.
-- For more complex scenarios different 'Dictionary' indexes could be used here too.
start :: HasCallStack => BaseType -> String -> Dictionary -> Guide -> Completer
start !ix !prefix !dict !guide  =
#ifdef trace
  unsafePerformIO do
    traceIO $ concat [ "-completer:start ix ", show ix, " prefix ", prefix, " l ", show $ length prefix ]
    pure $!
#endif
      let !gsize = guideSize guide
          prefix' = Vector.map (fromIntegral @_ @UCharType . ord) $ Vector.fromList prefix

          !nc = Completer
           { completerDictionary = dict
           , completerGuide = guide
           , completerKey = runST do
               let l = Vector.length prefix'
               v <- Vector.unsafeThaw prefix'
               v' <- VM.grow v 1
               VM.unsafeWrite v' l 0
               Vector.unsafeFreeze v'
           , completerIndexStack = if gsize /= 0 then Elem ix EndOfStack else EndOfStack
           , completerLastIndex = 0
           }
      in nc
{-# INLINE start #-}

-- | Retrieves next completion.
-- If present, 'Completer' will be returned. 'Nothing', otherwise.
next :: HasCallStack => Completer -> Maybe Completer
next !c =
  let withNonEmptyStack comp action = case completerIndexStack comp of
        EndOfStack -> Nothing
        Elem !ix _rest -> action ix comp
      {-# INLINE withNonEmptyStack #-}
        
      withNonRootLastIndex !ix !comp action =
        case completerLastIndex comp /= Dict.root of
          True -> action ix comp
          False -> findTerminal ix comp
      {-# INLINE withNonRootLastIndex #-}

      withChildLabel !ix comp =
#ifdef trace
        unsafePerformIO do
          let !childLabel = fromIntegral . Guide.child ix $! completerGuide comp
          putStrLn $ concat
            [ "next ix ", show ix, " lastIx ", show $ completerLastIndex comp
            , " childLabel ", show childLabel]
          pure $!
#else
          let !childLabel = fromIntegral . Guide.child ix $! completerGuide comp in
#endif
            if childLabel /= 0
              then followTerminal childLabel ix comp
              else go ix comp
      {-# INLINE withChildLabel #-}

      go :: HasCallStack => BaseType -> Completer -> Maybe Completer
      go !ix' !c' =
        let !siblingLabel = fromIntegral . Guide.sibling ix' $! completerGuide c'
            !ksize = Vector.length (completerKey c')
            !nkey = if ksize > 1
              then runST do
                v <- Vector.unsafeThaw . Vector.init . Vector.init $! completerKey c'
                v' <- VM.grow v 1
                VM.unsafeWrite v' (ksize - 2) 0
                Vector.unsafeFreeze v'
              else completerKey c'
            -- drop last element
            !nstack = case completerIndexStack c' of
              EndOfStack -> EndOfStack
              Elem !_ix !rest -> rest
            !nc = c' { completerKey = nkey, completerIndexStack = nstack }
        in case nstack of
          EndOfStack -> Nothing
          Elem !pix !_rest -> if siblingLabel /= 0
            then followTerminal siblingLabel pix nc
            else go pix nc

      followTerminal !label' !ix' !c' =
        case follow label' ix' c' of
          Nothing -> Nothing
          Just (!nextIx, !nextC) -> findTerminal nextIx nextC
      {-# INLINE followTerminal #-}

      nextByIx !ix comp =
#ifdef trace
        unsafePerformIO do
          traceIO $ concat
            [ "next ix ", show ix, " lastIx ", show $ completerLastIndex comp]
          pure $!
#endif
            withNonRootLastIndex ix comp withChildLabel
      {-# INLINE nextByIx #-}
  in withNonEmptyStack c nextByIx
{-# INLINE next #-}

-- | Retrieves a completion result from 'Completer' as 'String'.
keyToString :: HasCallStack => Completer -> String
keyToString = fmap (chr . fromIntegral) . safeInit . Vector.toList . completerKey
  where
    safeInit [] = []
    safeInit xs = init xs
    {-# INLINE safeInit #-}
{-# INLINE keyToString #-}
    
-- | Retrieves a value associated
-- with the last visited index by 'Completer' from the 'Dictionary'.
value :: HasCallStack => Completer -> ValueType
value c = Dict.value (completerLastIndex c) (completerDictionary c)
{-# INLINE value #-}

-- | Retrieve all completion results by given @prefix@
-- from 'Dictionary' via associated 'Guide'. Consider following lexicon:
--
-- @
--   a
--   an
--   and
--   appear
--   apple
--   bin
--   can
--   cat
-- @
--
-- Once dictionary and guide are ready, call 'completeKeys':
--
-- >>> completeKeys "a" dict guide
-- ["a", "an", "and", "appear", "apple"]
--
completeKeys :: String -> Dictionary -> Guide -> [String]
completeKeys prefix dict guide = 
  let !l = fromIntegral $ length prefix
      goDict acc !dictIx =
        case Dict.followPrefixLength prefix l dictIx dict of
          Nothing -> acc
          Just !nextDictIx ->
            let !nc = start nextDictIx "" dict guide
                !nacc = goNext acc nc
            in goDict nacc nextDictIx
      goNext acc !comp = case next comp of
        Nothing -> acc
        Just !nc ->
          let !nextWord = concat [prefix, keyToString nc]
              !nacc = nextWord : acc
          in goNext nacc nc
  in goDict [] Dict.root
{-# INLINE completeKeys #-}

-- | Apply completer to entire lexicon, starting with a function
-- that will handle dictionary traversal following the first character of the word.
completeLexicon
  :: forall a. (Char -> Completer -> a)
  -> Dictionary
  -> Guide
  -> [a]
completeLexicon completerSelector dict guide =
  let goDict dict' guide' !firstChar =
        let prefix = chr $ fromIntegral firstChar
        in case Dict.followChar firstChar 0 dict' of
          Nothing -> []
          Just !nextDictIx ->
            let !nc = start nextDictIx "" dict' guide'
            in goNext [] prefix nc

      goNext acc prefix comp = case next comp of
        Nothing -> acc
        Just !nc ->
          let !next' = completerSelector prefix nc
              !nacc = next' : acc
          in goNext nacc prefix nc

  -- FIXME: optimise it even further
  in concatMap  (goDict dict guide) [(1 :: CharType) .. 127]
{-# INLINE completeLexicon #-}

-- | Traverses the entire DAWG, returns only words in no particular order.
keys :: Dictionary -> Guide -> [String]
keys dict guide =
  let selectWord prefix comp = prefix : keyToString comp
  in completeLexicon selectWord dict guide

-- | Traverses the entire DAWG, returns only values associated with words.
-- Considering the unboxed nature of dictionary units, if there is no value associated with
-- a word, it returns @0@.
values :: Dictionary -> Guide -> [ValueType]
values dict guide =
  let selectValue _prefix = value
  in completeLexicon selectValue dict guide

-- | Traverses the entire DAWG, returns list of pairs
-- where key is a word and value is 32-bit integer.
-- In absence of value, @0@ will be returned.
toList :: Dictionary -> Guide -> [(String, ValueType)]
toList dict guide =
  let selectPair prefix comp =
        let !k = prefix : keyToString comp
            !v = value comp
        in (k, v)
  in completeLexicon selectPair dict guide

-- ** Helpers

-- | Helper function that is being used in 'next'.
-- Follows a given char within the 'Dictionary'
-- and performs necessary transformations to 'Completer'.
follow :: HasCallStack => CharType -> BaseType -> Completer -> Maybe (BaseType, Completer)
follow !label !ix !c =
  case Dict.followChar label ix (completerDictionary c) of
    Nothing -> Nothing
    Just !nextIx ->
      let !ksize = Vector.length (completerKey c)
          !oldStack = completerIndexStack c
          !nkey = appendKeyLabel label ksize (completerKey c)
          !nc = c { completerKey = nkey, completerIndexStack = Elem nextIx oldStack }
      in Just (nextIx, nc)
{-# INLINE follow #-}

-- | Helper function to identify the terminal occurence.
-- Returns 'Completer' if there was an occurence. 'Nothing', otherwise.
findTerminal :: HasCallStack => BaseType -> Completer -> Maybe Completer
findTerminal !ix !c
  | Dict.hasValue ix (completerDictionary c) =
      let !nc = c { completerLastIndex = ix }
      in Just nc
  | otherwise =
      let !label = fromIntegral $! Guide.child ix $! completerGuide c
      in case Dict.followChar label ix (completerDictionary c) of
           Nothing -> Nothing
           Just !nextIx ->
             let !ksize = Vector.length (completerKey c)
                 !oldStack = completerIndexStack c
                 !nkey = appendKeyLabel label ksize (completerKey c)
                 !nc = c { completerKey = nkey
                         , completerIndexStack = Elem nextIx oldStack
                         }
             in findTerminal nextIx nc

appendKeyLabel :: CharType -> Int -> Vector UCharType -> Vector UCharType
appendKeyLabel !label !ksize !key = runST do
  v <- Vector.unsafeThaw key
  let (!delta, !deltaIx) = if ksize >= 1 then (1, ksize - 1) else (2, ksize)
  v' <- VM.grow v delta
  VM.unsafeWrite v' deltaIx $ fromIntegral label
  VM.unsafeWrite v' (deltaIx + 1) 0
  Vector.unsafeFreeze v'
{-# INLINE appendKeyLabel #-}

-- | Dump completer state to stdout.
dump :: HasCallStack => String -> Completer -> IO ()
dump prefix Completer{..} = do
  let msg = unlines $ fmap ((prefix <> " ") <>)
        [ "ksize " <> (show $ Vector.length $ completerKey) <> " " <> show completerKey
        , concat
            ["issize ", (show $ size $ completerIndexStack), " ", show completerIndexStack]
        , "lastIx " <> show completerLastIndex
        ]
  putStrLn msg
