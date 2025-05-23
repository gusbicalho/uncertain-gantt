# WALKTHROUGH.md

## Overview

Uncertain Gantt is a probabilistic project planning tool that helps estimate project completion times when task durations are uncertain. It uses Monte Carlo simulation to generate statistical insights about project timelines, taking into account resource constraints and task dependencies.

## How It Works

### 1. Domain-Specific Language (DSL)

The application reads a custom DSL (`.ug` files) that describes projects. Here's an example:

```
resource Dev 2
resource QA 1

duration short = uniform 1 3
duration medium = uniform 3 7
duration long = normal 10 2

task "Design API" Dev medium
task "Implement API" Dev long
  after "Design API"
task "Write Tests" Dev short
  after "Design API"
task "Integration Testing" QA medium
  after "Implement API"
  after "Write Tests"

run 1000
```

### 2. Core Concepts

#### Resources
Resources represent workers, equipment, or any limited capacity:
- Each resource has a name and capacity (e.g., `Dev 2` means 2 developers)
- Tasks consume resources while executing
- Resource constraints affect project scheduling

#### Duration Distributions
Tasks have uncertain durations modeled as probability distributions:
- **Uniform(min, max)**: Equal probability between min and max days
- **Normal(mean, stddev)**: Bell curve distribution
- **LogNormal(mean, stddev)**: Skewed distribution for tasks that occasionally take much longer

#### Tasks
Tasks are the work units:
- Have a name, description, required resource, and duration
- Can depend on other tasks (`after` clause)
- Cannot start until dependencies complete and resources are available

### 3. Application Architecture

#### Parsing Layer
- **Parser.hs**: Megaparsec-based parser converts DSL text to structured `Statement` types
- **Types.hs**: Defines AST for the DSL (Statement, TaskDescription, DurationD, etc.)

#### Domain Layer
- **Task.hs**: Core task entity with dependencies
- **Project.hs**: Aggregates tasks and resources, validates consistency:
  - Checks resources exist
  - Resolves task dependencies
  - Detects dependency cycles

#### Simulation Layer
- **Simulator.hs**: Monte Carlo simulation engine
  - Samples random durations from distributions
  - Schedules tasks respecting dependencies and resource constraints
  - Uses `mostDependentsFirst` prioritization
  - Runs multiple simulations to build statistical picture

#### Presentation Layer
- **Gantt.hs**: Generates text-based Gantt charts and statistics
- **Stats.hs**: Calculates percentiles and creates histograms
- **ToText.hs**: Formats output for display

### 4. Execution Flow

1. **Input Processing**:
   - Parse DSL statements from file or interactive input
   - Build up project state incrementally

2. **Project Building**:
   - Each statement updates the interpreter state
   - `AddResource` creates resources
   - `AddTask` creates tasks with dependencies
   - `DefineDuration` creates reusable duration aliases

3. **Simulation**:
   - `Run N` triggers N Monte Carlo simulations
   - Each simulation:
     - Samples random durations for all tasks
     - Schedules tasks based on availability
     - Tracks completion times

4. **Output Generation**:
   - `PrintGantt` shows one sample timeline
   - `PrintProjectCompletionTimes` shows statistical summary
   - `PrintHistogram` visualizes completion time distribution

### 5. Streaming Architecture

The application uses a streaming-based architecture:

- Uses `streaming` library for composable output generation
- `StatementInterpreter` typeclass separates computation from I/O
- `ConsoleInterpreter` implements streaming output as `Stream (Of Text) IO`
- Output can be consumed flexibly (console, file, testing, etc.)
- Better separation of concerns between business logic and presentation

### 6. Key Algorithms

#### Task Scheduling (Simulator.hs)
```haskell
1. Initialize: All tasks pending, time = 0
2. While tasks remain:
   a. Find ready tasks (dependencies complete, resources available)
   b. Prioritize by number of dependents
   c. Schedule highest priority tasks that fit resources
   d. Advance time to next task completion
   e. Free resources from completed tasks
```

#### Dependency Resolution (Project.hs)
- Builds dependency graph
- Topological sort to detect cycles
- Maps task names to task objects

### 7. Example Workflow

Using `resources/example.ug`:

1. **Define Resources**: `resource Dev 2` creates 2 developer slots
2. **Define Tasks**: Tasks with durations and dependencies
3. **Run Simulation**: `run 1000` performs 1000 simulations
4. **View Results**:
   - Gantt chart shows one possible timeline
   - Statistics show likely completion times (50th, 90th percentiles)
   - Histogram visualizes probability distribution

### 8. Error Handling

The system validates:
- Resources exist before tasks reference them
- Dependencies form a valid DAG (no cycles)
- Task names are unique
- Referenced tasks exist

Errors are reported with clear messages indicating the problem.
