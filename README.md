# dawgdic

## Overview

**dawgdic** is a library for building and accessing 
dictionaries implemented with directed acyclic word 
graphs (DAWG).

This is a ported version of [C++ dawgdic library](https://github.com/s-yata/dawgdic).

## Features

This library offers `DAWG`, `Dictionary`, `Guide` and `Completer` data types as well as their builders.

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

Add following snippet to your `cabal.project`.

```
source-repository-package
  type: git
  location: https://github.com/swamp-agr/dawgdic.git
  tag: <commit>
```

Add `dawgdic` dependency to your project and run `cabal build`.


## Building and Querying


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
10/dawgdic.follow                    22.83
10/dawg.follow                       84.73
10/dawg-ord.follow                  112.70
10/packed-dawg.follow               153.57
100/dawgdic.follow                  342.51
100/dawg.follow                    1390.66
100/dawg-ord.follow                1853.18
100/packed-dawg.follow             2253.21
1000/dawgdic.follow                3755.68
1000/dawg.follow                  19082.06
1000/dawg-ord.follow              21964.74
1000/packed-dawg.follow           30714.20
------------------------------ -----------
10/dawgdic.lookup value              24.82
10/dawg.lookup value                 58.22
10/dawg-ord.lookup value            213.32
100/dawgdic.lookup value            330.57
100/dawg.lookup value               833.03
100/dawg-ord.lookup value          3212.93
1000/dawgdic.lookup value          4070.23
1000/dawg.lookup value            11787.74
1000/dawg-ord.lookup value        36521.00
------------------------------ -----------
10/dawgdic.member                    23.76
10/dawg.member                       56.37
10/dawg-ord.member                  184.54
10/packed-dawg.member               154.27
100/dawgdic.member                  354.90
100/dawg.member                     856.64
100/dawg-ord.member                2842.97
100/packed-dawg.member             2248.56
1000/dawgdic.member                8335.12
1000/dawg.member                  10020.16
1000/dawg-ord.member              32508.10
1000/packed-dawg.member           31006.43
------------------------------ -----------
10/dawgdic.keys                     180.86
10/dawg.keys                        100.74
10/dawg-ord.keys                    161.75
10/packed-dawg.keys                  49.61
100/dawgdic.keys                   2314.76
100/dawg.keys                      1412.84
100/dawg-ord.keys                  2261.92
100/packed-dawg.keys                720.42
1000/dawgdic.keys                 28035.17
1000/dawg.keys                    16735.86
1000/dawg-ord.keys                28050.78
1000/packed-dawg.keys              7561.71
------------------------------ -----------
10/dawgdic.values                   123.79
10/dawg.values                       71.34
10/dawg-ord.values                  138.60
100/dawgdic.values                 1519.01
100/dawg.values                    1084.41
100/dawg-ord.values                1869.49
1000/dawgdic.values               21219.50
1000/dawg.values                  11604.29
1000/dawg-ord.values              21996.63
------------------------------ -----------
10/dawgdic.toList                   179.85
10/dawg.toList                      115.07
10/dawg-ord.toList                  200.56
100/dawgdic.toList                 2358.35
100/dawg.toList                    1696.74
100/dawg-ord.toList                2773.02
1000/dawgdic.toList               41068.60
1000/dawg.toList                  18312.91
1000/dawg-ord.toList              35826.00
------------------------------ -----------
10/dawgdic.complete word            418.32
10/dawg.complete word               260.42
10/packed-dawg.complete word        311.15
100/dawgdic.complete word          7897.64
100/dawg.complete word             4765.40
100/packed-dawg.complete word      5762.11
1000/dawgdic.complete word       110815.38
1000/dawg.complete word           71789.43
1000/packed-dawg.complete word    71823.28
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
