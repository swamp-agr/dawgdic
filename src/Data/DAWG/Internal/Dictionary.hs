{-# LANGUAGE CPP #-}
module Data.DAWG.Internal.Dictionary where

import Control.DeepSeq (NFData)
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
  } deriving (Generic, Binary, NFData)

totalSize :: Dictionary -> SizeType
totalSize d = DictionaryUnit.size * dictionarySize d 
{-# INLINE totalSize #-}

fileSize :: Dictionary -> SizeType
fileSize d = baseTypeSize + totalSize d
{-# INLINE fileSize #-}

root :: BaseType
root = 0
{-# INLINE root #-}

hasValue :: HasCallStack => BaseType -> Dictionary -> Bool
hasValue !ix d = DictionaryUnit.hasLeaf (dictionaryUnits d UV.! fromIntegral ix)
{-# INLINE hasValue #-}

value :: HasCallStack => BaseType -> Dictionary -> ValueType
value !ix d = DictionaryUnit.value (dictionaryUnits d UV.! fromIntegral oix)
  where
    !oix = ix .^. DictionaryUnit.offset (dictionaryUnits d UV.! fromIntegral ix)
{-# INLINE value #-}

read :: HasCallStack => FilePath -> IO Dictionary
read = Binary.decodeFile

write :: HasCallStack => FilePath -> Dictionary -> IO ()
write = Binary.encodeFile

contains :: HasCallStack => String -> Dictionary -> Bool
contains !key d =
  case follow key root d of
    Nothing -> False
    Just ix -> hasValue ix d
{-# INLINE contains #-}

containsPrefixLength :: HasCallStack => String -> SizeType -> Dictionary -> Bool
containsPrefixLength !k !l d =
  case followPrefixLength k l root d of
    Nothing -> False
    Just ix -> hasValue ix d
{-# INLINE containsPrefixLength #-}

lookup :: HasCallStack => String -> Dictionary -> Maybe ValueType
lookup !k d =
  case follow k root d of
    Nothing -> Nothing
    Just ix -> DictionaryUnit.value <$> (dictionaryUnits d UV.!? fromIntegral ix)
{-# INLINE lookup #-}

lookupPrefixLength :: HasCallStack => String -> SizeType -> Dictionary -> Maybe ValueType
lookupPrefixLength !k !l d =
  case followPrefixLength k l root d of
    Nothing -> Nothing
    Just ix -> DictionaryUnit.value <$> (dictionaryUnits d UV.!? fromIntegral ix)
{-# INLINE lookupPrefixLength #-}

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
{-# INLINE followChar #-}

follow :: HasCallStack => String -> BaseType -> Dictionary -> Maybe BaseType
follow [] !ix _d = Just ix
follow (!c : !cs) !ix d = if ord c == fromIntegral (ord '\0')
  then Just ix
  else case followChar (fromIntegral . ord $ c) ix d of
         Nothing -> Nothing
         Just !nextIx -> follow cs nextIx d
{-# INLINE follow #-}

followPrefixLength
  :: HasCallStack => String -> SizeType -> BaseType -> Dictionary -> Maybe BaseType
followPrefixLength !cs !l !ix d =
    let follow' !ix' [] = Just ix'
        follow' !ix' (!i : !is) =
#ifdef trace
          tracePure (concat ["-followLength ix ", show i, " l ", show l]) $!
#endif
              case followChar (fromIntegral $ ord (cs !! fromIntegral i)) ix' d of
                Nothing -> Nothing
                Just !nIx -> follow' nIx is
    in follow' ix [0 .. pred l]
{-# INLINE followPrefixLength #-}

dump :: HasCallStack => Dictionary -> IO ()
dump d = do
  putStrLn "dictionary"
  putStrLn $ concat ["i\tu(", show $ dictionarySize d, ")"]
  
  forM_ [0 .. (dictionarySize d) - 1] \ix -> do
    putStrLn $ concat
      [show ix, "\t", show $ dictionaryUnits d UV.! fromIntegral ix]
