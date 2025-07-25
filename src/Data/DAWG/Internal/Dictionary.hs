{-# LANGUAGE CPP #-}
module Data.DAWG.Internal.Dictionary where

import Control.Monad (forM_)
import Data.Binary
import Data.Bits
import Data.Char
import Data.Vector.Binary ()
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import Data.DAWG.Internal.BaseType
import Data.DAWG.Internal.DictionaryUnit (DictionaryUnit)

import qualified Data.Binary as Binary
import qualified Data.Vector.Unboxed as UV

import qualified Data.DAWG.Internal.DictionaryUnit as DictionaryUnit

#ifdef trace
import Data.DAWG.Trace
#endif

-- ** Dictionary

data Dictionary = Dictionary
  { dictionaryUnits :: UV.Vector DictionaryUnit
  , dictionarySize :: SizeType
  } deriving (Generic, Binary)

totalSize :: Dictionary -> SizeType
totalSize d = DictionaryUnit.size * dictionarySize d 

fileSize :: Dictionary -> SizeType
fileSize d = baseTypeSize + totalSize d

root :: BaseType
root = 0

hasValue :: HasCallStack => BaseType -> Dictionary -> Bool
hasValue !ix d = DictionaryUnit.hasLeaf (dictionaryUnits d UV.! fromIntegral ix)

value :: HasCallStack => BaseType -> Dictionary -> ValueType
value !ix d = DictionaryUnit.value (dictionaryUnits d UV.! fromIntegral oix)
  where
    !oix = ix .^. DictionaryUnit.offset (dictionaryUnits d UV.! fromIntegral ix)

read :: HasCallStack => FilePath -> IO Dictionary
read = Binary.decodeFile

write :: HasCallStack => FilePath -> Dictionary -> IO ()
write = Binary.encodeFile

contains :: HasCallStack => String -> Dictionary -> Bool
contains !key d =
  case follow ((fromIntegral . ord) <$> key) root d of
    Nothing -> False
    Just ix -> hasValue ix d

containsLength :: HasCallStack => [CharType] -> SizeType -> Dictionary -> Bool
containsLength !k !l d =
  case followLength k l root d of
    Nothing -> False
    Just ix -> hasValue ix d

find :: HasCallStack => [CharType] -> Dictionary -> Maybe ValueType
find !k d =
  case follow k root d of
    Nothing -> Nothing
    Just ix -> DictionaryUnit.value <$> (dictionaryUnits d UV.!? fromIntegral ix)

findLength :: HasCallStack => [CharType] -> SizeType -> Dictionary -> Maybe ValueType
findLength !k !l d =
  case followLength k l root d of
    Nothing -> Nothing
    Just ix -> DictionaryUnit.value <$> (dictionaryUnits d UV.!? fromIntegral ix)

followChar :: HasCallStack => CharType -> BaseType -> Dictionary -> Maybe BaseType
followChar !l !ix d =
  let !u = dictionaryUnits d UV.! fromIntegral ix
      !nextIx = (ix .^. DictionaryUnit.offset u) .^. fromIntegral l
      !nu = dictionaryUnits d UV.! fromIntegral nextIx
#ifdef trace
      !debugStr = concat
        [ "-follow l ", show $ chr $ fromIntegral l
        , " ix ", show ix, " nextIx ", show nextIx
        , " u ", show u, " nu ", show nu
        ]
#endif
  in
#ifdef trace
      tracePure debugStr $
#endif
        if DictionaryUnit.label nu /= fromIntegral l then Nothing else Just nextIx

follow :: HasCallStack => [CharType] -> BaseType -> Dictionary -> Maybe BaseType
follow [] !ix _d = Just ix
follow (!c : !cs) !ix d = if c == fromIntegral (ord '\0')
  then Just ix
  else case followChar c ix d of
         Nothing -> Nothing
         Just !nextIx -> follow cs nextIx d

followCount
  :: HasCallStack => [CharType] -> BaseType -> SizeType -> Dictionary -> (SizeType, Maybe BaseType)
followCount [] !ix !acc _d = (acc, Just ix)
followCount (!c : !cs) !ix !acc d = if c == fromIntegral (ord '\0')
  then (acc, Just ix)
  else case followChar c ix d of
         Nothing -> (acc, Nothing)
         Just !nextIx -> followCount cs nextIx (succ acc) d

followLength
  :: HasCallStack => [CharType] -> SizeType -> BaseType -> Dictionary -> Maybe BaseType
followLength !cs !l !ix d =
    let follow' !ix' [] = Just ix'
        follow' !ix' (!i : !is) =
#ifdef trace
          tracePure (concat ["-followLength ix ", show i, " l ", show l]) $!
#endif
              case followChar (cs !! fromIntegral i) ix' d of
                Nothing -> Nothing
                Just !nIx -> follow' nIx is
    in follow' ix [0 .. pred l]

followLengthCount
  :: HasCallStack => [CharType]
  -> SizeType
  -> BaseType
  -> SizeType
  -> Dictionary
  -> (SizeType, Maybe BaseType)
followLengthCount !cs !l !ix !acc d =
    let follow' !ix' !acc' [] = (acc', Just ix')
        follow' !ix' !acc' (!i : !is) =
          case followChar (cs !! fromIntegral i) ix' d of
            Nothing -> (acc', Nothing)
            Just !nIx -> follow' nIx (succ acc) is
    in follow' ix acc [0 .. pred l]

dump :: HasCallStack => Dictionary -> IO ()
dump d = do
  putStrLn "dictionary"
  putStrLn $ concat ["i\tu(", show $ dictionarySize d, ")"]
  
  forM_ [0 .. (dictionarySize d) - 1] \ix -> do
    putStrLn $ concat
      [show ix, "\t", show $ dictionaryUnits d UV.! fromIntegral ix]
