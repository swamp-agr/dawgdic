module Data.DAWG.Internal.RankedGuide where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import GHC.Generics

import Data.DAWG.Internal.BaseType
import Data.DAWG.Internal.Dictionary
import Data.DAWG.Internal.Guide (Guide (..))

import qualified Data.Binary as Binary

import qualified Data.DAWG.Internal.Guide as G

newtype RankedGuide = RankedGuide { rankedGuide :: Guide }
  deriving newtype (Binary, NFData)
  deriving stock (Generic)

child :: BaseType -> RankedGuide -> UCharType
child !ix = G.child ix . rankedGuide

sibling :: BaseType -> RankedGuide -> UCharType
sibling !ix = G.sibling ix . rankedGuide

rankedGuideDictionary :: RankedGuide -> Dictionary
rankedGuideDictionary = guideDictionary . rankedGuide

read :: FilePath -> IO RankedGuide
read = Binary.decodeFile

write :: FilePath -> RankedGuide -> IO ()
write = Binary.encodeFile

dump :: RankedGuide -> IO ()
dump = G.dump . rankedGuide
