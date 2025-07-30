{-# LANGUAGE BlockArguments #-}
module Data.DAWG.BuildSpec where

import qualified Data.DAWG.DAWG as DAWG
import qualified Data.DAWG.Dictionary as Dict

import Control.Monad (forM_)
import Data.Maybe (isJust)
import Test.Hspec

import qualified Data.Vector as Vector

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "DAWG" do
    it "DAWG builds from words, DAWG contains words" do
      let insert' builder (k, ml, result) = do
            res <- case ml of
              Nothing -> DAWG.insert (Vector.fromList k) Nothing builder
              Just l  -> DAWG.insertWithLength (Vector.fromList k) l 0 builder
            res `shouldBe` result
          contains' dict w = Dict.contains w dict `shouldBe` True
          dictDataset = ["apple", "cherry", "durian", "green", "mandarin"]
          dawgDataset =
            [ ("apple", Nothing, True)
            , ("cherry", Nothing, True)
            , ("banana", Nothing, False)
            , ("durian", Nothing, True)
            , ("green\0apple", Just 11, False)
            , ("green\0apple", Nothing, True)
            , ("mandarin orange", Just 8, True)
            , ("mandarin", Nothing, True)
            ]
      db <- DAWG.new
      forM_  dawgDataset \entry -> do
        insert' db entry
      dawg <- DAWG.freeze db

      mDictB <- Dict.build dawg
      isJust mDictB `shouldBe` True
      forM_ mDictB \dictB -> do
        dict <- Dict.freeze dictB
        forM_ dictDataset \entry -> do
          contains' dict entry
