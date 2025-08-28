# dawgdic

## Overview

**dawgdic** is a library for building and accessing 
dictionaries implemented with directed acyclic word 
graphs (DAWG).

This is a ported version of [C++ dawgdic library](https://github.com/s-yata/dawgdic).

## Features

This library offers `DAWG`, `Dictionary`, `Guide` and `Completer` data types as well as their builders. 

- `DAWG` represents word graph. It is provided as intermediate data structure.
- `Dictionary` represents compact layout for `DAWG`. It offers API to check the presence of the word and associated value in the graph. Also it could be stored into a file and loaded from file.
- `Guide` is being used as a tree-like index to get the faster navigation through the dictionary. Also could be stored into a file and loaded from file.

Input characters must be in range 0-255.

This port does not contain `RankedGuide` and `RankedCompleter` (yet).

### Comparison with other DAWG libraries

- dictionary file size, in bytes:

| package/lexicon size (words)   | 2.6K   | 26K     | 370K     |
| ------------------------------ | ------ | ------- | -------- |
| dawg: Data.DAWG.Dynamic        | 370868 | 2823660 | 29933964 |
| dawg: Data.DAWG.Static         | 133138 | 1030578 | 11128690 |
| dawg-ord: Data.DAWG.Int        | N/A    | N/A     | N/A      |
| dawg-ord: Data.DAWG.Ord        | N/A    | N/A     | N/A      |
| packed-dawg: Data.DAWG.Packed  | 15619  | 128491  | 1481671  |
| dawgdic: Data.DAWG.Dictionary  | 3088   | 141328  | 1603600  |
| dawgdic: Guide (w/ Dictionary) | 4640   | 212000  | 2405408  |

- features:

| feature          | dawg    | dawg-ord | packed-dawg  | dawgdic |
| ---------------- | ------- | -------- | ------------ | ------- |
| build from list  | +       | +        | +            | +       |
| persistence      | +       | -        | +            | +       |
| insert           | Dynamic | +        | -            | Builder |
| delete key       | Dynamic | +        | -            | -       |
| follow character | Static  | ~edges   | lookupPrefix | +       |
| lookup value     | +       | +        | -            | +       |
| member           | ~lookup | ~lookup  | +            | +       |
| keys             | +       | +        | +            | +       |
| values           | +       | +        | -            | +       |
| toList (assocs)  | +       | +        | -            | +       |
| complete word    | ~submap | -        | ~toList      | +       |
| fuzzy search     | -       | -        | -            | -       |
| max size         | N/A     | N/A      | 2^22 nodes   | N/A     |


## Installation

Add `dawgdic` dependency to your project and run `cabal build`.


## Building and Querying

Building DAWG from lexicon of words and ignoring insertion failures is as simple as this:

```haskell
>>> import Data.DAWG.DAWG
>>> dawg <- fromAscList . lines =<< readFile "/path/to/lexicon"
```

Otherwise, consider mutable builder. It could also be useful if words are associated with values.

```haskell
buildOrError content = do
  dawgBuilder <- new
  forM_ content \(word, value) -> do
    result <- insert word (Just value) dawgBuilder
    unless result $ error "Insert failed"
  freeze dawgBuilder
```

Building dictionary and guide:

```
>>> import qualified Data.DAWG.Dictionary as D
>>> import qualified Data.DAWG.Guide as G
>>> dict <- D.build' dawg
>>> guide <- G.build' dawg dict
```

Saving dictionary and guide:

```
>>> D.write "dict.dawg" dict
>>> G.write "guide.dawg" guide
```

Loading dictionary and guide:

```
>>> dict <- D.load "dict.dawg"
>>> guide <- G.load "guide.dawg"
```

Consider using `Completer` for auto-complete-like queries or if you need to obtain lexicon back from `Dictionary` and `Guide`.

## Benchmarks

### Utilities

```
Benchmark                             default(μs)
------------------------------------- ------------
Utilities/10/Dawg.fromAscList             5549.76
Utilities/10/Dict.build'                 26981.64
Utilities/10/Dict.contains                  22.46
Utilities/10/Dict.lookup                    22.68
Utilities/10/Dict.follow                    22.42
Utilities/10/Guide.build'                   76.81
Utilities/10/Completer.completeKeys        385.35
------------------------------------- ------------
Utilities/100/Dawg.fromAscList           66486.21
Utilities/100/Dict.build'               194881.38
Utilities/100/Dict.contains                326.16
Utilities/100/Dict.lookup                  323.45
Utilities/100/Dict.follow                  319.86
Utilities/100/Guide.build'                 782.24
Utilities/100/Completer.completeKeys      7016.36
------------------------------------- ------------
Utilities/1000/Dawg.fromAscList         888061.61
Utilities/1000/Dict.build'             1659798.44
Utilities/1000/Dict.contains              3627.54
Utilities/1000/Dict.lookup                3638.10
Utilities/1000/Dict.follow                3564.64
Utilities/1000/Guide.build'               7992.73
Utilities/1000/Completer.completeKeys    82343.20
------------------------------------- ------------
```

### How to reproduce:

- Install `bench-show`:

```
cabal install bench-show --overwrite-policy=always
```

- Run and wait (it might take around `3m` to complete):

```
time cabal bench +RTS "-N4 -A64m -n4m -qb0" -RTS  --benchmark-options="--output bench.html --csv results.csv"
```

- Generate report:

```
bench-show --presentation=Solo report results.csv
```

### Comparison

```
Benchmark                      default(μs)
------------------------------ -----------
10/dawgdic.follow                    21.04
10/dawg.follow                       85.41
10/dawg-ord.follow                  110.42
10/packed-dawg.follow               147.30
100/dawgdic.follow                  311.05
100/dawg.follow                    1367.81
100/dawg-ord.follow                1774.78
100/packed-dawg.follow             2098.12
1000/dawgdic.follow                3273.02
1000/dawg.follow                  18842.47
1000/dawg-ord.follow              21992.03
1000/packed-dawg.follow           28938.01
------------------------------ -----------
10/dawgdic.lookup value              21.74
10/dawg.lookup value                 61.89
10/dawg-ord.lookup value            209.93
100/dawgdic.lookup value            290.13
100/dawg.lookup value               847.27
100/dawg-ord.lookup value          2973.35
1000/dawgdic.lookup value          3551.44
1000/dawg.lookup value            14217.29
1000/dawg-ord.lookup value        40111.28
10/dawgdic.member                    21.02
------------------------------ -----------
10/dawg.member                       56.43
10/dawg-ord.member                  191.74
10/packed-dawg.member               173.97
100/dawgdic.member                  343.32
100/dawg.member                     873.70
100/dawg-ord.member                2863.96
100/packed-dawg.member             2200.33
1000/dawgdic.member                9288.22
1000/dawg.member                  13878.72
1000/dawg-ord.member              31428.47
1000/packed-dawg.member           29089.83
------------------------------ -----------
10/dawgdic.keys                     108.33
10/dawg.keys                        100.23
10/dawg-ord.keys                    160.51
10/packed-dawg.keys                  50.50
100/dawgdic.keys                   1392.78
100/dawg.keys                      1413.00
100/dawg-ord.keys                  2324.99
100/packed-dawg.keys                692.23
1000/dawgdic.keys                 15628.83
1000/dawg.keys                    16541.79
1000/dawg-ord.keys                27612.77
1000/packed-dawg.keys              7415.48
------------------------------ -----------
10/dawgdic.values                    52.26
10/dawg.values                       72.29
10/dawg-ord.values                  134.79
100/dawgdic.values                  616.02
100/dawg.values                    1060.87
100/dawg-ord.values                1794.82
1000/dawgdic.values                6457.32
1000/dawg.values                  11734.25
1000/dawg-ord.values              21532.54
------------------------------ -----------
10/dawgdic.toList                   107.26
10/dawg.toList                      116.89
10/dawg-ord.toList                  202.35
100/dawgdic.toList                 1367.87
100/dawg.toList                    1608.26
100/dawg-ord.toList                2898.26
1000/dawgdic.toList               16334.34
1000/dawg.toList                  18578.59
1000/dawg-ord.toList              36360.44
------------------------------ -----------
10/dawgdic.complete word            367.97
10/dawg.complete word               248.14
10/packed-dawg.complete word        303.69
100/dawgdic.complete word          6151.51
100/dawg.complete word             4494.85
100/packed-dawg.complete word      4913.83
1000/dawgdic.complete word        71152.49
1000/dawg.complete word           58222.76
1000/packed-dawg.complete word    60828.53
------------------------------ -----------
```

### How to reproduce:

- Install `bench-show`:

```
cabal install bench-show --overwrite-policy=always
```

- Run and wait (it might take around `7m` to complete):

```
time cabal bench +RTS "-N4 -A64m -n4m -qb0" -RTS  --benchmark-options="--output bench.html --csv results.csv"
```

- Generate report:

```
bench-show --presentation=Solo report results.csv
```

## Contributing

In cases of issues please attach callstack, provide minimal dictionary lexicon and provide logs with enabled tracing.

```
cabal build -ftrace
```

## Acknowledgments

- [Susumu Yata](https://github.com/s-yata) as original author of C++ library.
