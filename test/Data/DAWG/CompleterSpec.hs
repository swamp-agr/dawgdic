{-# LANGUAGE BlockArguments #-}
module Data.DAWG.CompleterSpec where

import Data.DAWG.Internal.BaseType

import Data.DAWG.Completer (keyToString, start, next)

import qualified Data.DAWG.Completer as C
import qualified Data.DAWG.Guide as G
import qualified Data.DAWG.DAWG as DAWG
import qualified Data.DAWG.Dictionary as Dict

import Control.Monad (forM_)
import Data.Maybe (fromMaybe, isJust)
import Test.Hspec
import Text.Read (readMaybe)

import qualified Data.Vector as Vector

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Completer" do
    it "Builds a completer from a lexicon" do
      db <- DAWG.new
      contents <- readFile "data/lexicon"
      forM_ (lines contents) \l -> do
        let (w, strVal) = break (== '\t') l
            mVal = readMaybe . drop 1 $ strVal
            value = fromMaybe maxBound mVal :: ValueType
        DAWG.insert (Vector.fromList w) (Just value) db
      dawg <- DAWG.freeze db
      -- it must not fail since the DictionarySpec covers it
      dict <- Dict.build' dawg
      mGuide <- G.build dawg dict
      isJust mGuide `shouldBe` True
      forM_ mGuide \guide -> G.write "lexicon.dic" guide
    
    it "Completes keys from a lexicon" do
      let completerResultFile = "completer-result"
          goNext w c = case next c of
            Nothing -> pure ()
            Just !nc -> do
              appendFile completerResultFile $ concat
                [ " ", w, keyToString nc, " = ", show $ C.value nc ]
              goNext w nc

          go :: BaseType -> String -> String -> G.Guide -> IO ()
          go _dictIx _fullWord [] _guide = pure ()
          go dictIx fw w g = do
            let d = G.guideDictionary g
            case Dict.followPrefixLength w (fromIntegral $ length w) dictIx d of
              Nothing -> pure ()
              Just nextDictIx -> do
                let !nc = start nextDictIx "" g
                goNext fw nc
                go nextDictIx fw w g

      writeFile completerResultFile ""
      guide <- G.read "lexicon.dic"
      
      contents <- readFile "data/query"
      forM_ (lines contents) \l -> do
        appendFile completerResultFile $ concat [ l, ":" ]
        go Dict.root l l guide
        appendFile completerResultFile "\n"

      True `shouldBe` True

    it "Checks results" do
      let completerResultFile = "completer-result"
      testResult <- readFile completerResultFile
      testExpectation <- readFile "data/completer-answer"
      testResult `shouldBe` testExpectation
