{-# LANGUAGE BlockArguments #-}
module Data.DAWG.DictionarySpec where

import Data.DAWG.Internal.BaseType

import qualified Data.DAWG.Internal.DAWGBuilder as DB
import qualified Data.DAWG.Internal.DictionaryBuilder as DiB
import qualified Data.DAWG.Internal.Dictionary as Dict

import Control.Monad (forM_, when)
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Test.Hspec
import Text.Read (readMaybe)

import qualified Data.Vector as Vector

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Dictionary" do
    it "Builds a dictionary from a lexicon" do
      db <- DB.new
      contents <- readFile "data/lexicon"
      forM_ (lines contents) \l -> do
        let (w, strVal) = break (== '\t') l
            mVal = readMaybe . drop 1 $ strVal
            value = fromMaybe maxBound mVal :: ValueType
        DB.insert (Vector.fromList w) (Just value) db
      dawg <- DB.finish db
      dib <- DiB.new dawg
      dibResult <- DiB.build dib
      dict <- DiB.finish dib
      Dict.write "lexicon.dic" dict
      dibResult `shouldBe` True
    
    it "Finds prefix keys from a lexicon" do
      let dictResultFile = "dictionary-result"
      
          go _dictIx _word [] _dict = pure ()
          go dictIx w ((k, ix) : rest) d = do
            case Dict.followChar k dictIx d of
              Nothing -> pure ()
              Just nextDictIx -> do
                when (Dict.hasValue nextDictIx d) do
                  appendFile dictResultFile $ concat
                    [ " ", take (ix + 1) w, " = ", show $ Dict.value nextDictIx d, ";" ]
                go nextDictIx w rest d

      writeFile dictResultFile ""
      dict <- Dict.read "lexicon.dic"
      
      contents <- readFile "data/query"
      forM_ (lines contents) \l -> do
        appendFile dictResultFile $ concat [ l, ":" ]
        let queries = zip ((fromIntegral . ord) <$> l) [0 .. length l - 1]
        go Dict.root l queries dict
        appendFile dictResultFile "\n"

      True `shouldBe` True

    it "Checks results" do
      let dictResultFile = "dictionary-result"
      testResult <- readFile dictResultFile
      testExpectation <- readFile "data/dictionary-answer"
      testResult `shouldBe` testExpectation
