# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Development Commands

- `cabal build` - Build the project
- `cabal run uncertain-gantt -- [args]` - Run the executable
- `cabal test` - Run tests (minimal test suite currently)
- `cabal repl` - Open GHCi with the library loaded
- `./release.sh` - Build and package releases for distribution
- `cabal exec -- fourmolu -i src app` - Format Haskell source files

## Architecture Overview

This is a probabilistic project planning tool that uses Monte Carlo simulation to estimate task durations and project timelines.

### Key Architectural Patterns

1. **DSL-based Interface**: Custom domain-specific language (`.ug` files) parsed with megaparsec
2. **Streaming Architecture**: Uses streaming-based interpreter with Text output for composable I/O
3. **Probabilistic Modeling**: Uses monad-bayes for Monte Carlo simulation of task durations
4. **State Management**: InterpreterState manages project state, separate from presentation logic

### Module Structure

- `UncertainGantt.Script.*` - DSL parsing and interpretation
- `UncertainGantt.Project` - Core domain model (tasks, resources, projects)
- `UncertainGantt.Simulator` - Monte Carlo simulation engine
- `UncertainGantt.Gantt` - Visualization and statistics
- `UncertainGantt` module - Exports public APIs for the library

### Important Dependencies

- `monad-bayes` - Pinned to specific commit in cabal.project for probabilistic modeling
- `megaparsec` - Parsing DSL
- `time` - Date/time handling
- `text` - Text processing throughout

## DSL Language Features

The tool supports:
- Task definitions with probabilistic durations (uniform, normal, log-normal distributions)
- Resource allocation and management
- Task dependencies
- Interactive REPL commands (run, save, etc.)
- Statistical analysis of simulation results

Example scripts are in `resources/` directory.