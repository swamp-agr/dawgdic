{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.Exception (evaluate)
import Criterion.Main (bench, bgroup, defaultMain, nf, nfIO)
import Data.List (sort)
import Data.Maybe (listToMaybe)

import qualified Data.DAWG.DAWG as Dawg
import qualified Data.DAWG.Dictionary as Dict
import qualified Data.DAWG.Guide as G
import qualified Data.DAWG.Completer as C

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

-- | @fullLexiconPath@ should nbe the relative or absolute local path to the file @words_alpha.txt@
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

completerCompleteKeysBench !dict !guide lexicon =
  let go !w = C.completeKeys w dict guide
  in concatMap go lexicon

utilities n = do
  !lexiconN <- evaluate =<< readLexicon n
  !dawg <- evaluate =<< Dawg.fromAscList lexiconN
  !dict <- evaluate =<< Dict.build' dawg
  !guide <- evaluate =<< G.build' dawg dict
  return $ bgroup (show n)
    [ bench "Dawg.fromAscList" $ nfIO (dawgFromAscListBench lexiconN)
    , bench "Dict.build'" $ nfIO (Dict.build' dawg)
    , bench "Dict.contains" $ nf (dictContainsBench dict) lexiconN
    , bench "Dict.lookup" $ nf (dictLookupBench dict) lexiconN
    , bench "Dict.follow" $ nf (dictFollowBench dict) lexiconN
    , bench "Guide.build'" $ nfIO (G.build' dawg dict)
    , bench "Completer.completeKeys" $ nf (completerCompleteKeysBench dict guide) lexiconN
    ]

-- ** Main

main :: IO ()
main = do
  let inputs = [10, 100, 1000, 10000]
  utilitiesBench <- mapM utilities inputs
  defaultMain [ bgroup "Utilities" utilitiesBench ]
