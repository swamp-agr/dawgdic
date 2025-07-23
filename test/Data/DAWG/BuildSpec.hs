{-# LANGUAGE BlockArguments #-}
module Data.DAWG.BuildSpec where

import qualified Data.DAWG.Internal.DAWGBuilder as DB
import qualified Data.DAWG.Internal.DictionaryBuilder as DiB
import qualified Data.DAWG.Internal.Dictionary as Dict

import Control.Monad (forM_)
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
              Nothing -> DB.insert (Vector.fromList k) Nothing builder
              Just l  -> DB.insertWithLength (Vector.fromList k) l 0 builder
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
      db <- DB.new
      forM_  dawgDataset \entry -> do
        insert' db entry
      dawg <- DB.finish db

      dib <- DiB.new dawg
      dibResult <- DiB.build dib
      dibResult `shouldBe` True

      dict <- DiB.finish dib
      forM_ dictDataset \entry -> do
        contains' dict entry
