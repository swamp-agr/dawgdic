module Data.DAWG.Test where

import Data.DAWG.Internal.BaseType
import GHC.Stack

import Data.DAWG.Internal.Completer (Completer(..), keyToString, newCompleter, startCompleter, nextCompleter)

import qualified Data.DAWG.Internal.DAWGBuilder as DB
import qualified Data.DAWG.Internal.DictionaryBuilder as DiB
import qualified Data.DAWG.Internal.Dictionary as Dict
import qualified Data.DAWG.Internal.Completer as C
import qualified Data.DAWG.Internal.Guide as G
import qualified Data.DAWG.Internal.GuideBuilder as GB

import Control.Monad (forM_)
import Data.Char (ord)
import Data.Maybe (fromMaybe, isJust)
import Text.Read (readMaybe)

import qualified Data.Vector as Vector


test :: HasCallStack => IO ()
test = do
      let shouldBe a b = if a == b
            then putStrLn "Test passed: OK"
            else putStrLn $ concat [ "Test failed: Expected ", show a, "; got ", show b]

      db <- DB.new
      lcontents <- readFile "data/lexicon"
      forM_ (lines lcontents) \l -> do
        let (w, strVal) = break (== '\t') l
            mVal = readMaybe . drop 1 $ strVal
            value = fromMaybe maxBound mVal :: ValueType
        DB.insert (Vector.fromList w) (Just value) db
      dawg <- DB.finish db
      dib <- DiB.new dawg
      dibResult <- DiB.build dib
      dict0 <- DiB.finish dib
      Dict.dump dict0
      Dict.write "lexicon.dic" dict0
      dibResult `shouldBe` True

      mGuide <- GB.buildGuide =<< GB.newGuideBuilder dawg dict0
      isJust mGuide `shouldBe` True
      forM_ mGuide \guide -> G.write "lexicon.gde" guide

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
            putStrLn $ concat
              [ "completeKeys w ", w, " l ", show $ length w, " ix ", show dictIx]
            let c = newCompleter d g
            case Dict.followLength ((fromIntegral . ord) <$> w) (fromIntegral $ length w) dictIx d of
              Nothing -> pure ()
              Just nextDictIx -> do
                let !nc = startCompleter nextDictIx "" c
                C.dump "-completer:start" nc
                goNext fw nc
                go nextDictIx fw w d g

      writeFile completerResultFile ""
      dict <- Dict.read "lexicon.dic"
      guide <- G.read "lexicon.gde"
      G.dump guide
      
      contents <- readFile "data/query"
      forM_ (lines contents) \l -> do
        appendFile completerResultFile $ concat [ l, ":" ]
        go Dict.root l l dict guide
        appendFile completerResultFile "\n"


      True `shouldBe` True
