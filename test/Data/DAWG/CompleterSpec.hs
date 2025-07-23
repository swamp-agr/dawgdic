{-# LANGUAGE BlockArguments #-}
module Data.DAWG.CompleterSpec where

import Data.DAWG.Internal.BaseType

import Data.DAWG.Internal.Completer (Completer(..), keyToString, newCompleter, startCompleter, nextCompleter)

import qualified Data.DAWG.Internal.DAWGBuilder as DB
import qualified Data.DAWG.Internal.DictionaryBuilder as DiB
import qualified Data.DAWG.Internal.Dictionary as Dict
import qualified Data.DAWG.Internal.Completer as C
import qualified Data.DAWG.Internal.Guide as G
import qualified Data.DAWG.Internal.GuideBuilder as GB

import Control.Monad (forM_, when)
import Data.Char (ord)
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
      dibResult `shouldBe` True

      mGuide <- GB.buildGuide =<< GB.newGuideBuilder dawg dict
      isJust mGuide `shouldBe` True
      forM_ mGuide \guide -> G.write "lexicon.gde" guide
    
    it "Completes keys from a lexicon" do
      writeFile "completer-debug.txt" ""
      let completerResultFile = "completer-result"
          goNext w c = case nextCompleter c of
            Nothing -> pure ()
            Just !nc -> do
              appendFile "completer-debug.txt" $ concat
                [ "key = ", w, show $ completerKey nc, ": ", keyToString $ completerKey nc, "\n" ]
              appendFile completerResultFile $ concat
                [ " ", w, keyToString $ completerKey nc, " = ", show $ C.value nc ]
              goNext w nc

          go :: BaseType -> String -> String -> Dict.Dictionary -> G.Guide -> IO ()
          go _dictIx _fullWord [] _dict _guide = pure ()
          go dictIx fw w d g = do
            let c = newCompleter d g
            appendFile "completer-debug.txt" $ w <> "\n"
            case Dict.followLength ((fromIntegral . ord) <$> w) (fromIntegral $ length w) dictIx d of
              Nothing -> pure ()
              Just nextDictIx -> do
                appendFile "completer-debug" $ "ix " <> show nextDictIx <> "n"
                let !nc = startCompleter nextDictIx "" c
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
