# Uncertain Gantt Architecture

This document describes the key architectural aspects of the Uncertain Gantt project.

## Probabilistic Project Planning

Unlike traditional Gantt tools that use fixed task duration estimates, Uncertain Gantt models durations as probability distributions. This allows:

- Representing uncertainty explicitly via uniform, normal, and log-normal distributions
- Getting realistic project timeline distributions rather than single-point estimates
- Calculating confidence intervals for completion times (e.g., P50, P90)
- Making better decisions by understanding the probability of different outcomes

Implementation in: `UncertainGantt/Script/Duration.hs`, `UncertainGantt/Simulator.hs`

## Domain-Specific Language

The project implements a custom DSL for describing project structures with uncertainty:

```
resource Dev = 3
task "Backend API" {
  need Dev = 1
  duration = normal(10, 2) # Mean 10, standard deviation 2
}
task "Frontend" {
  need Dev = 2
  after "Backend API"
  duration = logNormal(3, 0.5)
}
```

The parser (built with Megaparsec) handles domain-specific error reporting and statement validation.

Implementation in: `UncertainGantt/Script/Parser.hs`, `UncertainGantt/Script/Types.hs`

## Interpreter

The project uses an interpreter typeclass to handle script statements:

- `StatementInterpreter` typeclass defines a common interface for interpreting statements
- Implementation is split between state management and presentation concerns
- `InterpreterState` handles state manipulation and core logic
- `ConsoleInterpreter` wraps InterpreterState and adds console output

Implementation in: `UncertainGantt/Script/StatementInterpreter.hs`, `UncertainGantt/Script/InterpreterState.hs`, `UncertainGantt/Script/ConsoleInterpreter.hs`

## Statement Handling Pattern

A simple and direct approach to handling different statement types:

- Uses pattern matching in the interpretStatement function with specific handlers
- Cleanly separates view (ConsoleInterpreter) from model (InterpreterState)
- Makes the code more maintainable and easier to understand
- Each statement type has its own dedicated handler function

This architecture separates scripting, simulation, and UI concerns cleanly.

## Monte Carlo Simulation

The project employs sophisticated simulation techniques:

- Uses monad-bayes for sampling from probability distributions
- Models resource contention with task prioritization
- Simulates dependent task execution with realistic scheduling constraints
- Aggregates results to generate statistical confidence intervals

The simulator accounts for real-world factors like resource limitations and task dependencies.

Implementation in: `UncertainGantt/Simulator.hs`, `UncertainGantt/Script/Stats.hs`
