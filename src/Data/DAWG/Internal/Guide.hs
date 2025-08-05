module Data.DAWG.Internal.Guide where

import Control.DeepSeq (NFData)
import Control.Monad (forM_)
import Data.Binary
import Data.Vector (Vector)
import Data.Vector.Binary ()
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import Data.DAWG.Internal.BaseType
import Data.DAWG.Internal.GuideUnit (GuideUnit)

import qualified Data.Binary as Binary
import qualified Data.Vector as Vector

import qualified Data.DAWG.Internal.GuideUnit as GuideUnit

-- ** Guide

data Guide = Guide
  { guideUnits :: Vector GuideUnit
  , guideSize :: SizeType
  } deriving (Generic, Binary, NFData)

empty :: Guide
empty = Guide
  { guideUnits = Vector.empty
  , guideSize = 0
  }

root :: BaseType
root = 0

child :: HasCallStack => BaseType -> Guide -> UCharType
child !ix !g = GuideUnit.child (guideUnits g Vector.! fromIntegral ix)

sibling :: HasCallStack => BaseType -> Guide -> UCharType
sibling !ix !g = GuideUnit.sibling (guideUnits g Vector.! fromIntegral ix)

read :: HasCallStack => FilePath -> IO Guide
read = Binary.decodeFile

write :: HasCallStack => FilePath -> Guide -> IO ()
write = Binary.encodeFile

dump :: HasCallStack => Guide -> IO ()
dump Guide{..} = do
  let gsize = Vector.length guideUnits
  putStrLn $ concat ["guide\t(", show gsize, ")"]
  forM_ [0 .. gsize - 1] \ix -> do
    putStrLn $ concat
      [show ix, "\t", show $ guideUnits Vector.! fromIntegral ix]
