{-# LANGUAGE BlockArguments #-}
module Data.DAWG.RankedCompleterSpec where

import Data.DAWG.Internal.BaseType

import Data.DAWG.RankedCompleter (keyToString, start, next)

import qualified Data.DAWG.RankedCompleter as C
import qualified Data.DAWG.RankedGuide as G
import qualified Data.DAWG.DAWG as DAWG
import qualified Data.DAWG.Dictionary as Dict

import Control.Monad (forM_, when)
import Data.Char (ord)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust)
import Test.Hspec
import Test.QuickCheck (chooseEnum, chooseInt, forAll, property, shuffle, vectorOf, withMaxSuccess)
import Text.Read (readMaybe)

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as Set
import qualified Data.Vector as Vector

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RankedCompleter" do
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
      forM_ mGuide \guide -> G.write "ranked-guide.dic" guide
    
    it "Completes keys from a lexicon" do
      let completerResultFile = "ranked-completer-result"
          goNext w c = case next c of
            Nothing -> pure ()
            Just !nc -> do
              appendFile completerResultFile $ concat
                [ " ", w, keyToString nc, " = ", show $ C.value nc ]
              goNext w nc

          go :: BaseType -> String -> String -> G.RankedGuide -> IO ()
          go _dictIx _fullWord [] _guide = pure ()
          go dictIx fw w g = do
            let d = G.rankedGuideDictionary g
            case Dict.followPrefixLength w (fromIntegral $ length w) dictIx d of
              Nothing -> pure ()
              Just nextDictIx -> do
                let !nc = start nextDictIx "" g
                goNext fw nc
                go nextDictIx fw w g

      writeFile completerResultFile ""
      guide <- G.read "ranked-guide.dic"
      
      contents <- readFile "data/query"
      forM_ (lines contents) \l -> do
        appendFile completerResultFile $ concat [ l, ":" ]
        go Dict.root l l guide
        appendFile completerResultFile "\n"

      True `shouldBe` True

    it "Checks results" do
      let completerResultFile = "ranked-completer-result"
      testResult <- readFile completerResultFile
      testExpectation <- readFile "data/ranked-completer-answer"
      testResult `shouldBe` testExpectation


    it "Completes randomly linked keys and values" do
      let n = 65536
          keyLength = 6

          whenJust x action = case x of
            Nothing -> pure ()
            Just val -> action val

          goNext c valueRef = do
            mPrevValue <- readIORef valueRef
            case next c of
              Nothing -> pure ()
              Just !nc -> do
                let v = C.value nc
                -- putStrLn $ concat [keyToString nc, ": ", show v]

                -- 2. Ranked value should be equal or less than the previous one
                -- (i.e. in descending order)
                whenJust mPrevValue \prevValue -> do
                  v `shouldSatisfy` (<= prevValue)

                writeIORef valueRef (Just v)

          wordGen = vectorOf keyLength $ chooseEnum ('A', 'Z')
          shuffledListN = do
            vals <- vectorOf n $ chooseInt (0, 100)
            keys <- vectorOf n wordGen
            let keys' = Set.toList . Set.fromList $ keys
            vals' <- shuffle vals
            pure $ zip keys' (fromIntegral <$> vals')
          prop_rankedCompleterCompletes = forAll shuffledListN \lexicon -> do
            db <- DAWG.new
            forM_ lexicon \(k, v) -> do
              -- putStrLn $ concat [k, "\t", show v]
              DAWG.insert (Vector.fromList k) (Just v) db
            dawg <- DAWG.freeze db
            dict <- Dict.build' dawg

            -- 1. Dictionary should contain value associated with a key
            forM_ lexicon \(k, v) -> do
              Dict.lookup k dict `shouldBe` Just v

            guide <- G.build' dawg dict
            globalRef <- newIORef (-1)
            forM_ ['A' .. 'Z'] \c -> do
              let firstLabel = fromIntegral $ ord c
              case Dict.followChar firstLabel Dict.root dict of
                Nothing -> pure ()
                Just dictIx -> do
                  prevValueRef <- newIORef Nothing
                  let !nc = start dictIx [c] guide
                  goNext nc prevValueRef
                  mLatestVal <- readIORef prevValueRef
                  globalVal <- readIORef globalRef
                  whenJust mLatestVal \latestVal -> do
                    when (globalVal < latestVal) do
                      writeIORef globalRef latestVal

            -- 3. maximum value should always be picked up during full dictionary traversal
            let maxVal = maximum (snd <$> lexicon)
            globalVal <- readIORef globalRef
            maxVal `shouldBe` globalVal
            
      withMaxSuccess 10 $ property prop_rankedCompleterCompletes

    it "Ensures binary compatibility with C++" do
      cppContents <- BSL.readFile "data/lexicon.ranked.dic"
      let cppGuide = Binary.decode cppContents
      guide <- G.read "ranked-guide.dic"
      cppGuide `shouldBe` guide
      Binary.encode guide `shouldBe` cppContents
