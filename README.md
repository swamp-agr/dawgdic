# dawgdic

## Overview

**dawgdic** is a library for building and accessing 
dictionaries implemented with directed acyclic word 
graphs (DAWG).

This is a ported version of [C++ dawgdic library](https://code.google.com/archive/p/dawgdic/).

## Features

This library offers `DAWG`, `Dictionary`, `Guide` and `Completer` data types as well as their builders.

## Installation

Add following snippet to your `cabal.project`.

```
source-repository-package
  type: git
  location: https://github.com/swamp-agr/dawgdic.git
  tag: 9412050fe9ca7201c3acd5b783cb976770033f86
```

Add `dawgdic` dependency to your project and run `cabal build`.


## Quick Start

## Building and Querying

## Benchmarks

```
Benchmark                             default(μs)
------------------------------------- ------------
Utilities/10/Dawg.fromAscList             23208.95
Utilities/10/Dict.build'                  28285.27
Utilities/10/Dict.contains                   23.50
Utilities/10/Dict.lookup                     23.97
Utilities/10/Dict.follow                     22.97
Utilities/10/Guide.build'                    80.87
Utilities/10/Completer.completeKeys         409.23
------------------------------------- ------------
Utilities/100/Dawg.fromAscList          4475571.24
Utilities/100/Dict.build'                201013.25
Utilities/100/Dict.contains                 342.69
Utilities/100/Dict.lookup                   342.90
Utilities/100/Dict.follow                   332.14
Utilities/100/Guide.build'                  817.70
Utilities/100/Completer.completeKeys       7385.04
------------------------------------- ------------
Utilities/1000/Dawg.fromAscList       186540604.54
Utilities/1000/Dict.build'              1698039.94
Utilities/1000/Dict.contains               3662.16
Utilities/1000/Dict.lookup                 3661.95
Utilities/1000/Dict.follow                 3600.80
Utilities/1000/Guide.build'                8045.64
Utilities/1000/Completer.completeKeys     85161.34
------------------------------------- ------------
```

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
