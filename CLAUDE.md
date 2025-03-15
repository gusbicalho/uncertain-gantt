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
- Structure: Hierarchical modules under UncertainGantt.*
- Extensions: Use DerivingStrategies, TypeApplications, ScopedTypeVariables

## Architecture
- Interpreter pattern: StatementInterpreter typeclass with implementations for ConsoleInterpreter and InterpreterState
- Separation of concerns: InterpreterState (state management) vs ConsoleInterpreter (UI/presentation)
- Simulation: Monte Carlo simulation using monad-bayes for probabilistic modeling
- Parser: Megaparsec-based parser for the custom DSL

## Error Handling
- Use algebraic data types for expected failures
- Support newtype pattern with explicit wrappers/unwrappers
- Use proper exception types for unexpected errors

## Common GHC Flags (from cabal file)
-Wall -Wcompat -Widentities -Wincomplete-uni-patterns -Wincomplete-record-updates