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

## Generic Visitor Pattern

A sophisticated type-level visitor pattern enables extensible operations over domain types:

- Uses GHC.Generics to decompose sum types structurally
- Provides type-safe pattern matching across modules without boilerplate
- Allows extending functionality without modifying existing types
- Uses qualified names at the type level to track type structure

Implementation in: `Utils/GenericVisitor.hs`, `Utils/QualifiedName.hs`

## Agent-Based Architecture

The codebase uses a flexible agent-based design:

- Type families associate agents with their action types
- Composable behavior through agent transformers
- Stateful and console-based agents for interactive use
- Abstract interfaces enabling different implementation strategies

This architecture separates scripting, simulation, and UI concerns cleanly.

Implementation in: `Utils/Agent/` directory, `UncertainGantt/Script/ConsoleAgent.hs`

## Monte Carlo Simulation

The project employs sophisticated simulation techniques:

- Uses monad-bayes for sampling from probability distributions
- Models resource contention with task prioritization
- Simulates dependent task execution with realistic scheduling constraints
- Aggregates results to generate statistical confidence intervals

The simulator accounts for real-world factors like resource limitations and task dependencies.

Implementation in: `UncertainGantt/Simulator.hs`, `UncertainGantt/Script/Stats.hs`