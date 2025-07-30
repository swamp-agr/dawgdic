{-# LANGUAGE CPP #-}
module Data.DAWG.Internal.Completer where

import Data.Char
import Data.Vector (Vector)
import GHC.Stack (HasCallStack)

import Data.DAWG.Internal.BaseType
import Data.DAWG.Internal.Dictionary (Dictionary (..))
import Data.DAWG.Internal.Guide (Guide (..))
import Data.DAWG.Internal.Stack

import qualified Data.Vector as Vector

import qualified Data.DAWG.Internal.Dictionary as Dict
import qualified Data.DAWG.Internal.Guide as Guide

#ifdef trace
import System.IO.Unsafe
import Data.DAWG.Trace
#endif


-- ** Completer

data Completer = Completer
  { completerDictionary :: Dictionary
  , completerGuide :: Guide
  , completerKey :: Vector UCharType
  , completerIndexStack :: Stack_ BaseType
  , completerLastIndex :: BaseType
  }

new :: Dictionary -> Guide -> Completer
new dict guide = Completer
  { completerDictionary = dict
  , completerGuide = guide
  , completerKey = Vector.empty
  , completerIndexStack = EndOfStack
  , completerLastIndex = 0
  }

start :: HasCallStack => BaseType -> String -> Completer -> Completer
start !ix !prefix !c =
#ifdef trace
  unsafePerformIO do
    traceIO $ concat [ "-completer:start ix ", show ix, " prefix ", prefix, " l ", show $ length prefix ]
    pure $!
#endif
      let !gsize = guideSize $ completerGuide c
          !nc = c
           { completerKey = Vector.fromList
             $ (fromIntegral @Int @UCharType . ord) <$> (prefix <> [chr 0])
           , completerIndexStack = if gsize /= 0 then Elem ix EndOfStack else EndOfStack
           , completerLastIndex = if gsize /= 0
              then Dict.root
              else 0 -- completerLastIndex c
           }
      in nc

next :: HasCallStack => Completer -> Maybe Completer
next !c =
  let withNonEmptyStack comp action = case completerIndexStack comp of
        EndOfStack -> Nothing
        Elem !ix !_rest -> action ix comp
        
      withNonRootLastIndex !ix comp action =
        case completerLastIndex comp /= Dict.root of
          True -> action ix comp
          False -> findTerminal ix comp

      withChildLabel !ix comp =
#ifdef trace
        unsafePerformIO do
          let !childLabel = fromIntegral $! Guide.child ix $! completerGuide comp
          putStrLn $ concat
            [ "next ix ", show ix, " lastIx ", show $ completerLastIndex comp
            , " childLabel ", show childLabel]
          pure $!
#else
          let !childLabel = fromIntegral $! Guide.child ix $! completerGuide comp in
#endif
            if childLabel /= 0
              then followTerminal childLabel ix comp
              else go ix comp

      go :: HasCallStack => BaseType -> Completer -> Maybe Completer
      go !ix' !c' =
        let !siblingLabel = fromIntegral $! Guide.sibling ix' $! completerGuide c'
            !ksize = Vector.length (completerKey c')
            !nkey = if ksize > 1
              then (Vector.init $! Vector.init $! completerKey c')
                `Vector.snoc` 0
              else completerKey c'
            -- drop last element
            !nstack = case completerIndexStack c' of
              EndOfStack -> EndOfStack
              Elem _ix rest -> rest
            !nc = c' { completerKey = nkey, completerIndexStack = nstack }
        in case nstack of
          EndOfStack -> Nothing
          Elem !pix !_rest -> if siblingLabel /= 0
            then followTerminal siblingLabel pix nc
            else go pix nc

      followTerminal label' ix' c' =
        case follow label' ix' c' of
          Nothing -> Nothing
          Just (!nextIx, !nextC) -> findTerminal nextIx nextC

      nextByIx ix comp =
#ifdef trace
        unsafePerformIO do
          traceIO $ concat
            [ "next ix ", show ix, " lastIx ", show $ completerLastIndex comp]
          pure $!
#endif
            withNonRootLastIndex ix comp withChildLabel

  in withNonEmptyStack c nextByIx


follow :: HasCallStack => CharType -> BaseType -> Completer -> Maybe (BaseType, Completer)
follow !label !ix !c =
  case Dict.followChar label ix (completerDictionary c) of
    Nothing -> Nothing
    Just !nextIx ->
      let !ksize = Vector.length (completerKey c)
          !oldStack = completerIndexStack c
          !nkey = if ksize >= 1
            then (Vector.init $! completerKey c)
              `Vector.snoc` fromIntegral label
              `Vector.snoc` 0
            else completerKey c
              `Vector.snoc` fromIntegral label
              `Vector.snoc` 0
          !nc = c { completerKey = nkey, completerIndexStack = Elem nextIx oldStack }
      in Just (nextIx, nc)

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
                 !nkey = if ksize >= 1
                   then (Vector.init $! completerKey c)
                     `Vector.snoc` fromIntegral label
                     `Vector.snoc` 0
                   else completerKey c
                     `Vector.snoc` fromIntegral label
                     `Vector.snoc` 0
                 !nc = c { completerKey = nkey
                         , completerIndexStack = Elem nextIx oldStack
                         }
             in findTerminal nextIx nc

keyToString :: HasCallStack => Vector UCharType -> String
keyToString = fmap (chr . fromIntegral) . safeInit . Vector.toList
  where
    safeInit [] = []
    safeInit xs = init xs
    
value :: HasCallStack => Completer -> ValueType
value c = Dict.value (completerLastIndex c) (completerDictionary c)

dump :: HasCallStack => String -> Completer -> IO ()
dump prefix Completer{..} = do
  let msg = unlines $ fmap ((prefix <> " ") <>)
        [ "ksize " <> (show $ length $ completerKey) <> " " <> show completerKey
        , "issize " <> (show $ size $ completerIndexStack) <> " " <> show completerIndexStack
        , "lastIx " <> show completerLastIndex
        ]
  putStrLn msg
