# dawgdic

## Overview

**dawgdic** is a library for building and accessing 
dictionaries implemented with directed acyclic word 
graphs (DAWG).

This is a ported version of [C++ dawgdic library](https://github.com/s-yata/dawgdic).

## Features

This library offers `DAWG`, `Dictionary`, `Guide` and `Completer` data types as well as their builders.

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

- Utilities:

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

### Comparison

- dictionary file size, in bytes:

| package/lexicon size (words)   | 2.6K   | 26K     | 370K     |
| ------------------------------ | ------ | ------- | -------- |
| dawg: Data.DAWG.Dynamic        | 370868 | 2823660 | 29933964 |
| dawg: Data.DAWG.Static         | 133138 | 1030578 | 11128690 |
| dawg-ord: Data.DAWG.Int        | N/A    | N/A     | N/A      |
| dawg-ord: Data.DAWG.Ord        | N/A    | N/A     | N/A      |
| packed-dawg: Data.DAWG.Packed  | 15619  | 128491  | 1481671  |
| dawgdic: Data.DAWG.Dictionary  | 3088   | 141328  | 1603600  |
| dawgdic: Guide (w/ Dictionary) | 4640   | 212000  | 1674272  |

- features:

| feature          | dawg    | dawg-ord | packed-dawg  | dawgdic |
| ---------------- | ------- | -------- | ------------ | ------- |
| build from list  | +       | +        | +            | +       |
| persistence      | +       | -        | +            | +       |
| insert           | Dynamic | +        | -            | +       |
| delete key       | Dynamic | +        | -            | -       |
| lookup index     | Static  | ~edges   | lookupPrefix | +       |
| follow character | -       | +        | -            | +       |
| lookup value     | +       | +        | -            | +       |
| member           | ~index  | ~lookup  | +            | +       |
| keys             | +       | +        | +            | -       |
| values           | +       | +        | -            | -       |
| assocs           | +       | +        | -            | -       |
| complete word    | ~submap | -        | ~toList      | +       |
| fuzzy search     | -       | -        | -            | -       |


### How to reproduce:

- Install `bench-show`:

```
cabal install bench-show --overwrite-policy=always
```

- Run and wait (it might take around `1h` to complete):

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
