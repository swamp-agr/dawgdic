{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (forM_, when)
import Data.Char (ord)
import qualified Data.Vector as Vector
import Options.Applicative
import Text.Read (readMaybe)

import qualified Data.DAWG.Internal.DAWGBuilder as DB
import Data.DAWG.Completer (keyToString, start, next)
import qualified Data.DAWG.Completer as C
import qualified Data.DAWG.DAWG as DAWG
import qualified Data.DAWG.Dictionary as Dict
import qualified Data.DAWG.Guide as Guide

main :: IO ()
main = execParser (info (commands <**> helper) idm) >>= run

-- ** CLI

data WithGuide = WithGuide | WithRankedGuide

data Opts = Opts
  { lexiconPath :: FilePath
  , tabSeparator :: Bool
  , withGuide :: Maybe WithGuide
  , dictionaryPath :: FilePath
  }

data Command = Build Opts | Find Opts

commands :: Parser Command
commands = subparser
  (command "build" (info buildOpts (progDesc "Build dictionary from lexicon"))
  <> (command "find" (info findOpts (progDesc "Find words in dictionary"))))
  where
    buildOpts = Build <$> opts
    findOpts = Find <$> opts
    opts = Opts
      <$> strOption
        (  long "lexicon"
        <> short 'l'
        <> metavar "FILE"
        <> help "Lexicon path")
      <*> switch
        (  long "tab-separated"
        <> short 't'
        <> help "Use tab as separator")
      <*> optional (guideOpts <|> rankedGuideOpts)
      <*> strOption
        (  long "dictionary"
        <> short 'd'
        <> metavar "FILE"
        <> help "Dictionary path")
    guideOpts = flag' WithGuide
      (long "guide" <> short 'g' <> help "Build dictionary with guide")
    rankedGuideOpts = flag' WithRankedGuide
      (long "ranked-guide" <> short 'r' <> help "Build dictionary with ranked-guide")
      
run :: Command -> IO ()
run = \case
  Build Opts{..} -> do
    contents <- readFile lexiconPath

    let check !n
          | n < 0 = 0
          | otherwise = n

        toKeyValue !line =
          let (w, strVal) = break (== '\t') line
              mVal = readMaybe . drop 1 $ strVal
          in (w, check <$> mVal)

        makeLexicon = if tabSeparator
          then fmap toKeyValue
          else fmap (\x -> (x, Nothing))

        lexicon = makeLexicon (lines contents)

    db <- DB.new
    forM_ lexicon \(w, mVal)  -> do
      DB.insert (Vector.fromList w) mVal db
    dawg <- DAWG.freeze db
    dict <- Dict.build' dawg

    case withGuide of
      Nothing -> Dict.write dictionaryPath dict
      Just WithGuide -> do
        guide <- Guide.build' dawg dict
        Guide.write ("guide." <> dictionaryPath) guide
      Just WithRankedGuide -> error "not implemented yet"
    
  Find Opts{..} -> do
    contents <- readFile lexiconPath
    case withGuide of
      Nothing -> do
        let go _dictIx _word [] _dict = pure ()
            go dictIx w ((k, ix) : rest) d = do
              case Dict.followChar k dictIx d of
                Nothing -> pure ()
                Just nextDictIx -> do
                  when (Dict.hasValue nextDictIx d) do
                    putStr $ concat
                      [ " ", take (ix + 1) w, " = ", show $ Dict.value nextDictIx d, ";" ]
                  go nextDictIx w rest d

        dict <- Dict.read dictionaryPath
        forM_ (lines contents) \l -> do
          putStr $ concat [l, ":"]
          let queries = zip ((fromIntegral . ord) <$> l) [0 .. length l - 1]
          go Dict.root l queries dict
          putStr "\n"

      Just WithGuide -> do
        let goNext w c = case next c of
              Nothing -> pure ()
              Just !nc -> do
                putStr $ concat
                  [ " ", w, keyToString nc, " = ", show $ C.value nc ]
                goNext w nc

            go _dictIx _fullWord [] _dict _guide = pure ()
            go dictIx fw w d g = do
              case Dict.followPrefixLength w (fromIntegral $ length w) dictIx d of
                Nothing -> pure ()
                Just nextDictIx -> do
                  let !nc = start nextDictIx "" d g
                  goNext fw nc
                  go nextDictIx fw w d g
        dict <- Dict.read dictionaryPath
        guide <- Guide.read $ "guide." <> dictionaryPath
        forM_ (lines contents) \l -> do
          putStr $ concat [l, ":"]
          go Dict.root l l dict guide
          putStr "\n"
        
      Just WithRankedGuide -> error "not implemented yet"
