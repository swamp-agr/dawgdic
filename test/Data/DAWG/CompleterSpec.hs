{-# LANGUAGE BlockArguments #-}
module Data.DAWG.CompleterSpec where

import Data.DAWG.Internal.BaseType

import Data.DAWG.Completer (Completer(..), keyToString, start, next)

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
      mDictB <- Dict.build dawg
      forM_ mDictB \dictBuilder -> do
        dict <- Dict.freeze dictBuilder
        Dict.write "lexicon.dic" dict
      isJust mDictB `shouldBe` True

      dict <- Dict.read "lexicon.dic"
      mGuide <- G.build dawg dict
      isJust mGuide `shouldBe` True
      forM_ mGuide \guide -> G.write "lexicon.gde" guide
    
    it "Completes keys from a lexicon" do
      writeFile "completer-debug.txt" ""
      let completerResultFile = "completer-result"
          goNext w c = case next c of
            Nothing -> pure ()
            Just !nc -> do
              appendFile "completer-debug.txt" $ concat
                [ "key = ", w, show $ completerKey nc, ": ", keyToString nc, "\n" ]
              appendFile completerResultFile $ concat
                [ " ", w, keyToString nc, " = ", show $ C.value nc ]
              goNext w nc

          go :: BaseType -> String -> String -> Dict.Dictionary -> G.Guide -> IO ()
          go _dictIx _fullWord [] _dict _guide = pure ()
          go dictIx fw w d g = do
            appendFile "completer-debug.txt" $ w <> "\n"
            case Dict.followPrefixLength w (fromIntegral $ length w) dictIx d of
              Nothing -> pure ()
              Just nextDictIx -> do
                appendFile "completer-debug" $ "ix " <> show nextDictIx <> "n"
                let !nc = start nextDictIx "" d g
                goNext fw nc
                go nextDictIx fw w d g

      writeFile completerResultFile ""
      dict <- Dict.read "lexicon.dic"
      guide <- G.read "lexicon.gde"
      
      contents <- readFile "data/query"
      forM_ (lines contents) \l -> do
        appendFile completerResultFile $ concat [ l, ":" ]
        go Dict.root l l dict guide
        appendFile completerResultFile "\n"

      True `shouldBe` True

    it "Checks results" do
      let completerResultFile = "completer-result"
      testResult <- readFile completerResultFile
      testExpectation <- readFile "data/completer-answer"
      testResult `shouldBe` testExpectation
