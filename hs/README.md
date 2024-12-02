# [Advent of Code 2024](https://adventofcode.com/2024)
### my answers in [Haskell](https://www.haskell.org/) ![Haskell CI](https://github.com/ephemient/aoc2024/workflows/Haskell%20CI/badge.svg)

This project builds with [The Haskell Cabal](https://www.haskell.org/cabal/).

Setup:

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install cabal latest
ghcup install ghc 9.10.1
cabal configure --with-compiler ghc-9.10 --enable-tests
```

Run the [Hspec](https://hspec.github.io/) test suite:

```sh
cabal test test:aoc2024-test
```

Run [criterion](http://www.serpentine.com/criterion/) benchmarks ([results online](https://ephemient.github.io/aoc2024/aoc2024-bench.html)):

```sh
cabal bench bench:aoc2024-bench
```

Print solutions for the inputs provided in local data files:

```sh
cabal run exe:aoc2024
```

Generate [Haddock](https://www.haskell.org/haddock/) API documentation:

```sh
cabal haddock lib:aoc2024
```

Run [hlint](https://github.com/ndmitchell/hlint) source code suggestions:

```sh
cabal install hlint
hlint src test bench
```

Run [ormolu](https://github.com/tweag/ormolu) formatting:

```sh
cabal install ormolu
git ls-files -coz '*.hs' | xargs -0 ormolu --mode inplace
```

Run [cabal-gild](https://github.com/tfausak/cabal-gild) formatting:

```sh
cabal install cabal-gild
cabal-gild -i aoc2024.cabal -o aoc2024.cabal
```
