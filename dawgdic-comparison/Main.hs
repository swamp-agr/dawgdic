{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad (forM, forM_)
import Criterion.Main (Benchmark, Benchmarkable, bench, bgroup, defaultMain, nf, nfIO)
import Data.List (sort)
import Data.Maybe (fromMaybe, isJust, isNothing)
import System.Directory (doesFileExist)

import qualified Text.Read as Text
import qualified Data.Vector as Vector

import qualified Data.DAWG.DAWG as Dawg
import qualified Data.DAWG.Dictionary as Dict
import qualified Data.DAWG.Guide as G
import qualified Data.DAWG.Completer as C

import qualified Data.DAWG.Static as D
import qualified Data.DAWG.Ord as DO
import qualified Data.DAWG.Packed as P

e = error "tbd"

-- ** Main

main :: IO ()
main = do
  let inputs = [10, 100, 1000]
  
  followGroup <- makeFollowGroup inputs
  lookupValueGroup <- makeLookupValueGroup inputs
  memberGroup <- makeMemberGroup inputs
  keysGroup <- makeKeysGroup inputs
  valuesGroup <- makeValuesGroup inputs
  toListGroup <- makeToListGroup inputs
  completeWordGroup <- makeCompleteWordGroup inputs

  defaultMain $ concat 
    [ followGroup
    , lookupValueGroup
    , memberGroup
    , keysGroup
    , valuesGroup
    , toListGroup
    , completeWordGroup
    ]

-- ** Configuration

data BenchItem weight val lex = BenchItem
  { benchItemGroup :: String
  , benchItemFun :: (Dict.Dictionary -> G.Guide -> [lex] -> Benchmarkable)
  , benchItemDawg :: Maybe (D.DAWG Char weight val -> [lex] -> Benchmarkable)
  , benchItemDawgOrd :: Maybe (DO.DAWG Char val -> [lex] -> Benchmarkable)
  , benchItemPackedDawg :: Maybe (P.Node -> [lex] -> Benchmarkable)
  , benchItemDawgBuilder :: [lex] -> D.DAWG Char weight val
  , benchItemDawgOrdBuilder :: [lex] -> DO.DAWG Char val
  , benchItemPackedDawgBuilder :: [lex] -> P.Node
  , benchItemTransformer :: String -> lex
  , benchItemBuilder :: [lex] -> IO Dawg.DAWG
  }

readMaybe transformer selector builder hasValue item size = case selector item of
  Nothing  -> pure Nothing
  Just fun -> do
    lexicon <- readLexicon transformer hasValue size
    let !dawg = (builder item) lexicon
    pure $! Just (fun, dawg)

-- ** Builders / Readers

readGuide transform dawgBuilder hasValue n = do
  let guidePath = concat ["data/guide.", show n, if hasValue then ".v" else "", ".dawg"]
  guideExists <- doesFileExist guidePath
  if guideExists
    then G.read guidePath
    else do
      lexicon <- readLexicon transform hasValue n
      dawg <- dawgBuilder lexicon
      dict <- Dict.build' dawg
      guide <- G.build' dawg dict
      G.write guidePath guide
      pure guide

readLexicon :: (String -> lex) -> Bool -> Int -> IO [lex]
readLexicon transform hasValue n = do
  let lexiconPath = concat ["data/lexicon.", show n, if hasValue then ".v" else "",  ".txt"]
  contents <- readFile lexiconPath
  pure $ fmap transform $ sort $ lines contents

strToPair str =
  let (w, strVal) = break (== '\t') str
      mVal = Text.readMaybe . drop 1 $ strVal
      val = fromMaybe maxBound mVal :: Int
  in (w, val)

fromKeyValueAscList lex = do
  db <- Dawg.new
  forM_ lex \(w, v) -> Dawg.insert (Vector.fromList w) (Just $ fromIntegral v) db
  Dawg.freeze db

-- ** Benchmark generators

generate :: BenchItem weight val lex -> [Int] -> IO [Benchmark]
generate item inps = forM inps \size -> do
  let groupName = benchItemGroup item
      fun = benchItemFun item
      withValues = isNothing (benchItemPackedDawg item)
      transform = benchItemTransformer item
      dawgBuilder = benchItemBuilder item

  lexiconN <- readLexicon transform withValues size

  -- dawgdic
  guide <- readGuide transform dawgBuilder withValues size
  let dict = G.guideDictionary guide

  -- dawg
  mDawgBench <- readMaybe transform benchItemDawg benchItemDawgBuilder withValues item size
  
  -- dawg-ord
  mDawgOrdBench <-
    readMaybe transform benchItemDawgOrd benchItemDawgOrdBuilder withValues item size

  -- packed-dawg
  mPackedDawgBench <-
    readMaybe transform benchItemPackedDawg benchItemPackedDawgBuilder withValues item size

  -- making benchmarks
  let finalGroupName = show size
      mkEntryName label = concat [label, ".", groupName]
      mkBench label (benchFun, dawg) =
        [ bench (mkEntryName label) $ benchFun dawg lexiconN ]
      mkBenchMaybe label = maybe [] (mkBench label)

  return $ bgroup finalGroupName $
    [ bench (mkEntryName "dawgdic") $ fun dict guide lexiconN ]
    <> mkBenchMaybe "dawg" mDawgBench
    <> mkBenchMaybe "dawg-ord" mDawgOrdBench
    <> mkBenchMaybe "packed-dawg" mPackedDawgBench

-- ** Follow / lookup index

followBenchItem :: BenchItem D.Weight () String
followBenchItem = BenchItem
  { benchItemGroup = "follow"
  , benchItemFun = followB
  , benchItemDawg = Just followDawgB
  , benchItemDawgOrd = Just followDawgOrdB
  , benchItemPackedDawg = Just followPackedDawgB
  , benchItemDawgBuilder = D.weigh . D.fromLang
  , benchItemDawgOrdBuilder = DO.fromLang
  , benchItemPackedDawgBuilder = P.fromAscList
  , benchItemTransformer = id
  , benchItemBuilder = Dawg.fromAscList
  }

makeFollowGroup = generate followBenchItem

followB dict _guide ws =
  let f !w = Dict.follow w Dict.root dict
  in nf (fmap f) ws

followDawgB dawg ws =
  let f !w = D.index w dawg
  in nf (fmap f) ws

followDawgOrdB dawg ws =
  let follow' !ix [] dawg = Just ix
      follow' !ix (c : cs) dawg = case DO.follow ix c dawg of
        Nothing -> Nothing
        Just nextIx -> follow' nextIx cs dawg

      f !w = follow' (DO.root dawg) w dawg
  in nf (fmap f) ws

followPackedDawgB dawg ws =
  let f !w = P.lookupPrefix w dawg
  in nf (fmap f) ws

-- ** Lookup value

lookupValueBenchItem :: BenchItem () Int (String, Int)
lookupValueBenchItem = BenchItem
  { benchItemGroup = "lookup value"
  , benchItemFun = lookupValueB
  , benchItemDawg = Just lookupValueDawgB
  , benchItemDawgOrd = Just lookupValueDawgOrdB
  , benchItemPackedDawg = Nothing
  , benchItemDawgBuilder = D.fromList
  , benchItemDawgOrdBuilder = DO.fromList
  , benchItemPackedDawgBuilder = P.fromAscList . fmap fst
  , benchItemTransformer = strToPair
  , benchItemBuilder = fromKeyValueAscList
  }

makeLookupValueGroup = generate lookupValueBenchItem

lookupValueB dict _guide ws =
  let f (!w, !v) = Dict.lookup w dict == Just (fromIntegral v)
  in nf (fmap f) ws

lookupValueDawgB dawg ws =
  let f (!w, !v) = D.lookup w dawg == Just v
  in nf (fmap f) ws

lookupValueDawgOrdB dawg ws =
  let f (!w, !v) = DO.lookup w dawg == Just v
  in nf (fmap f) ws
  

-- ** Member / Contains

memberBenchItem :: BenchItem () () String
memberBenchItem = BenchItem
  { benchItemGroup = "member"
  , benchItemFun = memberB
  , benchItemDawg = Just memberDawgB
  , benchItemDawgOrd = Just memberDawgOrdB
  , benchItemPackedDawg = Just memberPackedDawgB
  , benchItemDawgBuilder = D.fromLang
  , benchItemDawgOrdBuilder = DO.fromLang
  , benchItemPackedDawgBuilder = P.fromAscList
  , benchItemTransformer = id
  , benchItemBuilder = Dawg.fromAscList
  }

makeMemberGroup = generate memberBenchItem

memberB dict _guide ws =
  let f !w = Dict.member w dict
  in nf (fmap f) ws

memberDawgB dawg ws =
  let f !w = isJust $ D.lookup w dawg
  in nf (fmap f) ws

memberDawgOrdB dawg ws =
  let f !w = isJust $ DO.lookup w dawg
  in nf (fmap f) ws

memberPackedDawgB dawg ws =
  let f !w = P.member w dawg
  in nf (fmap f) ws

-- ** Keys

keysBenchItem :: BenchItem () () String
keysBenchItem = BenchItem
  { benchItemGroup = "keys"
  , benchItemFun = keysB
  , benchItemDawg = Just keysDawgB
  , benchItemDawgOrd = Just keysDawgOrdB
  , benchItemPackedDawg = Just keysPackedDawgB
  , benchItemDawgBuilder = D.fromLang
  , benchItemDawgOrdBuilder = DO.fromLang
  , benchItemPackedDawgBuilder = P.fromAscList
  , benchItemTransformer = id
  , benchItemBuilder = Dawg.fromAscList
  }

makeKeysGroup = generate keysBenchItem

keysB dict guide _ws = nf C.keys guide

keysDawgB dawg _ws = nf D.keys dawg

keysDawgOrdB dawg _ws = nf DO.keys dawg

keysPackedDawgB dawg _ws = nf P.toList dawg

-- ** Values

valuesBenchItem :: BenchItem () Int (String, Int)
valuesBenchItem = BenchItem
  { benchItemGroup = "values"
  , benchItemFun = valuesB
  , benchItemDawg = Just valuesDawgB
  , benchItemDawgOrd = Just valuesDawgOrdB
  , benchItemPackedDawg = Nothing
  , benchItemDawgBuilder = D.fromList
  , benchItemDawgOrdBuilder = DO.fromList
  , benchItemPackedDawgBuilder = P.fromAscList . fmap fst
  , benchItemTransformer = strToPair
  , benchItemBuilder = fromKeyValueAscList
  }

valuesB _dict guide _ws = nf C.values guide

valuesDawgB dawg _ws = nf D.elems dawg

valuesDawgOrdB dawg _ws = nf DO.elems dawg

makeValuesGroup = generate valuesBenchItem

-- ** toList / Assocs

toListBenchItem  :: BenchItem () Int (String, Int)
toListBenchItem = BenchItem
  { benchItemGroup = "toList"
  , benchItemFun = toListB
  , benchItemDawg = Just toListDawgB
  , benchItemDawgOrd = Just toListDawgOrdB
  , benchItemPackedDawg = Nothing
  , benchItemDawgBuilder = D.fromList
  , benchItemDawgOrdBuilder = DO.fromList
  , benchItemPackedDawgBuilder = P.fromAscList . fmap fst
  , benchItemTransformer = strToPair
  , benchItemBuilder = fromKeyValueAscList
  }

makeToListGroup = generate toListBenchItem

toListB dict guide _ws = nf C.toList guide

toListDawgB dawg _ws = nf D.assocs dawg

toListDawgOrdB dawg _ws = nf DO.assocs dawg

-- ** Complete words

completeWordBenchItem :: BenchItem D.Weight () String
completeWordBenchItem = BenchItem
  { benchItemGroup = "complete word"
  , benchItemFun = completeWordB
  , benchItemDawg = Just completeWordDawgB
  , benchItemDawgOrd = Nothing
  , benchItemPackedDawg = Just completeWordPackedDawgB
  , benchItemDawgBuilder = D.weigh . D.fromLang
  , benchItemDawgOrdBuilder = DO.fromLang
  , benchItemPackedDawgBuilder = P.fromAscList
  , benchItemTransformer = id
  , benchItemBuilder = Dawg.fromAscList
  }

makeCompleteWordGroup = generate completeWordBenchItem

completeWordB _dict guide ws =
  let f !w = C.completeKeys w guide
  in nf (concatMap f) ws

completeWordDawgB dawg ws =
  let f !w = D.keys $ D.submap w dawg
  in nf (concatMap f) ws

completeWordPackedDawgB dawg ws =
  let prepend !w !rest = concat [w, rest]
      f !w = case P.lookupPrefix w dawg of
        Nothing -> []
        Just node -> prepend w <$> P.toList node
  in nf (concatMap f) ws
