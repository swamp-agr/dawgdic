{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.Exception (evaluate)
import Criterion.Main (bench, bgroup, defaultMain, nf, nfIO)
import Data.List (sort)
import Data.Maybe (listToMaybe)

import qualified Data.DAWG.DAWG as Dawg
import qualified Data.DAWG.Dictionary as Dict
import qualified Data.DAWG.Guide as G
import qualified Data.DAWG.RankedGuide as RG
import qualified Data.DAWG.Completer as C
import qualified Data.DAWG.RankedCompleter as RC

-- ** Dataset preparation

-- | Helper function to generate @lexicon.<N>.txt@. Use 'generateAll' instead. It will take a minute to generate input for benchmarks.
generateN :: FilePath -> Int -> IO ()
generateN fullLexiconPath n = do
  let outputLexiconFile = concat ["lexicon.", show n, ".txt"]
      alphabet = ['a' .. 'z']

      go _ws [] = return ()
      go ws (char : restAlphabet) = do
        let allCurrent = takeWhile ((== (Just char)) . listToMaybe) ws
            current = take n allCurrent
            restWords = drop (length allCurrent) ws
        appendFile outputLexiconFile $ unlines current
        go restWords restAlphabet

  writeFile outputLexiconFile "" -- refresh
  words' <- lines <$> readFile fullLexiconPath
  go (sort words') alphabet

-- | Helper function to generate @lexicon.<N>.v.txt@ from @lexicon.<N>.txt@. Use 'generateAllValues' instead. It will take a couple minutes to generate input for comparison benchmarks.
generateValue :: FilePath -> Int -> IO ()
generateValue lexiconPath n = do
  contents <- lines <$> readFile lexiconPath
  let size = length contents
      values = reverse [1 .. size]
      toLine (w, v) = concat [w, "\t", show v]
      lexiconValPath = (<> ".v.txt") . reverse . drop 4 . reverse $ lexiconPath
  writeFile lexiconValPath $ unlines (toLine <$> zip contents values)

-- | @fullLexiconPath@ should be the relative or absolute local path to the file @words_alpha.txt@
-- from <https://github.com/dwyl/english-words/tree/master>.
-- Consider downloading and unpacking @words_alpha.zip@.
generateAll fullLexiconPath = mapM_ (generateN fullLexiconPath) [10, 100, 1000]

-- ** Utilities

-- | Benchmark inputs were produced via 'generateAll' function.
--
-- Revision: @20f5cc9@.
readLexicon :: Int -> IO [String]
readLexicon n = do
  let !lexiconFile = concat ["data/lexicon.", show n, ".txt"]
  lines <$> readFile lexiconFile

dawgFromAscListBench lexicon = do
  !_dawg <- Dawg.fromAscList lexicon
  pure ()

dictContainsBench !dict lexicon =
  let go dict !w = Dict.contains w dict
  in go dict <$> lexicon

dictLookupBench !dict lexicon =
  let go dict !w = Dict.lookup w dict
  in go dict <$> lexicon

dictFollowBench !dict lexicon =
  let go dict !w = Dict.follow w Dict.root dict
  in go dict <$> lexicon

completerCompleteKeysBench !guide lexicon =
  let go !w = C.completeKeys w guide
  in concatMap go lexicon

rankedCompleterCompleteKeysBench !guide lexicon =
  let go !w = RC.completeKeys w guide
  in concatMap go lexicon

utilities n = do
  !lexiconN <- evaluate =<< readLexicon n
  !dawg <- evaluate =<< Dawg.fromAscList lexiconN
  !dict <- evaluate =<< Dict.build' dawg
  !guide <- evaluate =<< G.build' dawg dict
  !rguide <- evaluate =<< RG.build' dawg dict
  return $ bgroup (show n)
    [ bench "Dawg.fromAscList" $ nfIO (dawgFromAscListBench lexiconN)
    , bench "Dict.build'" $ nfIO (Dict.build' dawg)
    , bench "Dict.contains" $ nf (dictContainsBench dict) lexiconN
    , bench "Dict.lookup" $ nf (dictLookupBench dict) lexiconN
    , bench "Dict.follow" $ nf (dictFollowBench dict) lexiconN
    , bench "Guide.build'" $ nfIO (G.build' dawg dict)
    , bench "Completer.completeKeys" $ nf (completerCompleteKeysBench guide) lexiconN
    , bench "RankedCompleter.completeKeys" $ nf (rankedCompleterCompleteKeysBench rguide) lexiconN
    ]

-- ** Main

main :: IO ()
main = do
  let inputs = [10, 100, 1000]
  utilitiesBench <- mapM utilities inputs
  defaultMain [ bgroup "Utilities" utilitiesBench ]
