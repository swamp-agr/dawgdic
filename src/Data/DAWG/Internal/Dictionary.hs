{-|
Module: Data.DAWG.Internal.Dictionary
Description: Exports dictionary as well as its internal API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.Internal.Dictionary where

import Control.DeepSeq (NFData)
import Control.Monad (forM_, when)
import Data.Binary
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import Data.Bits
import Data.Char
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import Data.DAWG.Internal.BaseType
import Data.DAWG.Internal.DictionaryUnit (DictionaryUnit (..))

import qualified Data.Binary as Binary
import qualified Data.Vector.Unboxed as UV

import qualified Data.DAWG.Internal.DictionaryUnit as DictionaryUnit

-- ** Dictionary

-- | Dictionary.
--
-- * Each unit is stored in 4 bytes.
-- * Size is stored in unsigned int.
data Dictionary = Dictionary
  { dictionaryUnits :: UV.Vector DictionaryUnit -- ^ Array of dictionary units.
  , dictionarySize :: SizeType -- ^ Size of the dictionary.
  } deriving (Generic, Eq, Show, NFData)

instance Binary Dictionary where
  get = do
    dictionarySize' <- getWord32le
    when (dictionarySize' < 256 || dictionarySize' `mod` 256 /= 0) do
      error $ "Invalid size: " <> show dictionarySize'
    dictionaryUnitsList <- getMany (get @DictionaryUnit)
      $ fromIntegral @BaseType @Int dictionarySize'
    let dictionarySize = fromIntegral @BaseType @SizeType dictionarySize'
        dictionaryUnits = UV.fromList dictionaryUnitsList
    pure Dictionary{..}
  put Dictionary{..} = do
    putWord32le $ fromIntegral @SizeType @BaseType dictionarySize
    mapM_ (put @DictionaryUnit) (UV.toList dictionaryUnits)

-- | Empty dictionary that does not contain any elements.
empty :: Dictionary
empty = Dictionary
  { dictionaryUnits = UV.empty
  , dictionarySize = 0
  }
{-# INLINE empty #-}

-- | Root dictionary index. Equivalent to @0@.
root :: BaseType
root = 0
{-# INLINE root #-}

-- | Checks whether dictionary unit has value by given index.
hasValue :: HasCallStack => BaseType -> Dictionary -> Bool
hasValue !ix d = DictionaryUnit.hasLeaf (dictionaryUnits d UV.! fromIntegral ix)
{-# INLINE hasValue #-}

-- | Gets a value of dictionary unit by given index.
value :: HasCallStack => BaseType -> Dictionary -> ValueType
value !ix d = DictionaryUnit.value (dictionaryUnits d UV.! fromIntegral oix)
  where
    !oix = ix .^. DictionaryUnit.offset (dictionaryUnits d UV.! fromIntegral ix)
{-# INLINE value #-}

-- | Load dictionary from a file.
read :: HasCallStack => FilePath -> IO Dictionary
read = Binary.decodeFile

-- | Save dictionary to a file.
write :: HasCallStack => FilePath -> Dictionary -> IO ()
write = Binary.encodeFile

-- | Alias for 'contains'.
member :: HasCallStack => String -> Dictionary -> Bool
member = contains
{-# INLINE member #-}

-- | Checks that the word contains in the dictionary.
contains :: HasCallStack => String -> Dictionary -> Bool
contains !key d =
  case follow key root d of
    Nothing -> False
    Just ix -> hasValue ix d
{-# INLINE contains #-}

-- | Similarly to 'contains' it checks that the word prefix
-- (provided as word and separate length)
-- contains in the dictionary. 
containsPrefixLength :: HasCallStack => String -> SizeType -> Dictionary -> Bool
containsPrefixLength !k !l d =
  case followPrefixLength k l root d of
    Nothing -> False
    Just !ix -> hasValue ix d
{-# INLINE containsPrefixLength #-}

-- | Performs lookup and retrieves value associated with the word
-- if it is present in the dictionary.
--
-- * If the word is contained in the dictionary but there is no value associated with it,
-- @Just 0@ will be returned.
-- * Otherwise, returns 'Nothing' if there is no such word in the dictionary.
lookup :: HasCallStack => String -> Dictionary -> Maybe ValueType
lookup !k d =
  case follow k root d of
    Nothing -> Nothing
    Just ix -> Just $! value ix d
{-# INLINE lookup #-}

-- | Similarlty to 'lookup', it performs lookup of the word prefix
-- (provided as word and separate length)
-- and retrieves value associated with the word
-- if it is present in the dictionary.
--
-- If the word is contained in the dictionary but there is no value associated with it,
-- @Just 0@ will be returned.
lookupPrefixLength :: HasCallStack => String -> SizeType -> Dictionary -> Maybe ValueType
lookupPrefixLength !k !l d =
  case followPrefixLength k l root d of
    Nothing -> Nothing
    Just !ix -> Just $! value ix d
{-# INLINE lookupPrefixLength #-}

-- | Follows the character by dictionary index of the previous character.
-- If there is a child unit, returns its index.
--
followChar :: HasCallStack => CharType -> BaseType -> Dictionary -> Maybe BaseType
followChar !l !ix d =
  let !lb = fromIntegral l
      !u = dictionaryUnits d UV.! fromIntegral ix
      !nextIx = (ix .^. DictionaryUnit.offset u) .^. lb
      !nu = dictionaryUnits d UV.! fromIntegral nextIx
  in
        if DictionaryUnit.label nu /= lb then Nothing else Just nextIx
{-# INLINE followChar #-}

-- | Recursively follows the word starting from its beginning.
-- If at any point there is no index while following is not finished, returns 'Nothing'.
-- Otherwise, returns a dictionary index associated with a last word character.
follow :: HasCallStack => String -> BaseType -> Dictionary -> Maybe BaseType
follow [] !ix _d = Just ix
follow (!c : cs) !ix d = if ord c == 0
  then Just ix
  else case followChar (fromIntegral . ord $ c) ix d of
         Nothing -> Nothing
         Just !nextIx -> follow cs nextIx d
{-# INLINE follow #-}

-- | Same as 'follow' but for word prefix.
followPrefixLength
  :: HasCallStack => String -> SizeType -> BaseType -> Dictionary -> Maybe BaseType
followPrefixLength !cs !l !ix d =
    let !cs' = UV.fromList (fmap (fromIntegral . ord) cs)
        follow' !ix' [] = Just ix'
        follow' !ix' (!i : is) =
              case followChar (cs' UV.! i) ix' d of
                Nothing -> Nothing
                Just !nIx -> follow' nIx is
    in follow' ix [0 .. pred (fromIntegral l)]
{-# INLINE followPrefixLength #-}

-- | Dump dictionary to stdout.
dump :: HasCallStack => Dictionary -> IO ()
dump d = do
  putStrLn "dictionary"
  putStrLn $ concat ["i\tu(", show $ dictionarySize d, ")"]
  
  forM_ [0 .. (dictionarySize d) - 1] \ix -> do
    putStrLn $ concat
      [show ix, "\t", show $ dictionaryUnits d UV.! fromIntegral ix]

-- ** Utilities

-- | Read exact amount of items from lazy bytestring by given helper.
getMany :: forall a. Binary a => Get a -> Int -> Get [a]
getMany getter n = go [] n
  where
    go xs 0 = return $! reverse xs
    go xs i = do
      x <- getter
      x `seq` go (x:xs) (i-1)
