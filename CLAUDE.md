# CLAUDE.md - Codebase Guide

## Build/Test Commands
- Build: `cabal build`
- Run: `cabal run uncertain-gantt`
- Test all: `cabal test`
- Single test: `cabal test --test-options='-p "PATTERN"'`

## Code Style
- Formatting: Fourmolu (2-space indent, leading commas, diff-friendly imports)
- Run formatter: `cabal exec -- fourmolu -i src app test`
- Naming: CamelCase for types, camelCase for functions, `un` prefix for newtype unwrappers
- Types: Explicit export lists, use newtypes with deriving strategies, algebraic data types
- Structure: Hierarchical modules under UncertainGantt.* and Utils.*
- Extensions: Use DerivingStrategies, TypeApplications, ScopedTypeVariables

## Error Handling
- Use algebraic data types for expected failures
- Support newtype pattern with explicit wrappers/unwrappers
- Use proper exception types for unexpected errors

## Common GHC Flags (from cabal file)
-Wall -Wcompat -Widentities -Wincomplete-uni-patterns -Wincomplete-record-updates