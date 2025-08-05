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
Benchmark                           default(μs)
----------------------------------- -----------
Utilities/10/Dawg.fromAscList          22218.73
Utilities/10/Dict.build'               23332.68
Utilities/10/Dict.contains                26.00
Utilities/10/Dict.lookup                  26.53
Utilities/10/Dict.follow                  25.32
Utilities/10/Guide.build'                558.97
Utilities/10/Completer.completeKeys      433.50
```

## Contributing

In cases of issues please attach callstack, provide minimal dictionary lexicon and provide logs with enabled tracing.

```
cabal build -ftrace
```

## Acknowledgments

- [Susumu Yata](https://github.com/s-yata) as original author of C++ library.
