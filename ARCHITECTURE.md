# Architecture

uncertain-gantt is a probabilistic project planner: task durations are
probability distributions, and project completion times are estimated by
Monte Carlo simulation under resource and dependency constraints. It
ships two frontends over one library — a CLI that runs `.ug` scripts
(with a REPL), and an interactive TUI editor.

## Layer map

```
┌─ Domain core (polymorphic in resource r and duration d) ────────────┐
│  Task        names (interned Symbols), description, deps            │
│  Project     validated container; BuildProjectM (strict builder)    │
│  Project.Tolerant   all-issues builder → (Project, [BuildIssue])    │
│  Simulator   scheduling simulation, pluggable prioritization        │
│  Gantt       schedules (task → period), rendering, completion time  │
│  ToText      Symbol/Text conversions used by every layer            │
└──────────────────────────────────────────────────────────────────────┘
┌─ Lang: the definition vocabulary ───┐  ┌─ Sim: simulation & stats ───┐
│  Lang.Types   Resource, DurationD,  │  │  Sim.Duration  distribution │
│               DurationAlias,        │  │      sampling (monad-bayes) │
│               TaskDescription,      │  │  Sim.Estimate  Monte Carlo  │
│               ResourceDescription   │  │      completion samples     │
│  Lang.Parser  expression parsers    │  │  Sim.Stats     weighted     │
│  Lang.Render  expression rendering  │  │      samples, quantiles,    │
└─────────────────────────────────────┘  │      histograms             │
                                         └─────────────────────────────┘
┌─ Storage formats ───────────────────────────────────────────────────┐
│  Toml          multi-project .toml files (primary; TOML-FORMAT.md)  │
│  Script.*      the .ug script format (legacy) + CLI interpreter:    │
│    Script.Types/Parser/Render        statements, file syntax        │
│    Script.{StatementInterpreter, InterpreterState,                  │
│            ConsoleInterpreter, Runner}   streaming interpreter      │
└──────────────────────────────────────────────────────────────────────┘
┌─ Frontends ─────────────────────────────────────────────────────────┐
│  app/   CLI: run scripts, REPL (uncertain-gantt)                    │
│  tui/   editor: Tui.{Doc, View, Widgets, Estimate}, Main            │
└──────────────────────────────────────────────────────────────────────┘
```

Dependency rules, enforced by imports (checkable with grep):

- The domain core imports nothing from `Lang`, `Sim`, `Script`, or `Toml`.
- `Lang` and `Sim` do not import `Script` or `Toml`.
- The TUI's only `Script` imports are the `.ug` persistence boundary
  (`parseScript`, `renderDeclarations`, `Statement`); swap those and the
  TUI knows nothing about scripts.

## Domain core

`Task r d` and `Project r d` are polymorphic in the resource and duration
types; the simulator takes duration semantics and scheduling policy as
plain functions:

```haskell
simulate :: Prioritization r d          -- Project -> doable -> ordering
         -> (d -> m Word)               -- duration estimator
         -> Project r d
         -> m (Gantt r d, Maybe unscheduled)
```

This is what lets frontends attach their own duration annotation (the
interpreter and TUI both use `d = (Maybe DurationAlias, DurationD)`) and
what keeps distributions, parsing and text out of the core. The simulator
reports incomplete schedules (`Maybe` of leftover tasks) rather than
failing; `mostDependentsFirst` is the only prioritization so far. The
simulation loop skips to the next task-completion time instead of ticking
day by day.

### Two ways to build a Project

- **`BuildProjectM`** (in `UncertainGantt.Project`): strict and
  sequential. Each `addTask` validates against the state built so far —
  missing resources, missing dependencies, cycles — and the first error
  aborts. Order matters: a task's dependencies must already be present.
  Used by the script interpreter, where statements arrive one at a time.
- **`Project.Tolerant`** (`TolerantBuild`): Validation-applicative style.
  Declarations are only *collected* (a Writer; order is irrelevant), and
  `runTolerantBuild` validates the whole set at once, returning **all**
  issues plus a `Project` containing everything usable. Tasks are
  excluded — each with a named `BuildIssue` — for undeclared resources,
  undeclared dependencies, cycle membership (SCCs via `Data.Graph`), or
  transitive dependence on an excluded task. Duplicate names keep the
  last declaration and report the shadowing. Used by the TUI.

Either way, an existing `Project` always satisfies the invariants: no
dangling references, no cycles. Two subtleties in `Project` worth knowing:
`addTask` only runs its cycle check when the task name already exists
(a brand-new task cannot create a cycle, since its dependencies must
already exist and cannot point forward to it), and
`transitiveDependents` is a lazily knot-tied map that would loop on a
cyclic graph — it is safe *only because* of that insert-time invariant.

## Lang: one vocabulary, three surfaces

`Lang.Types` is the abstract syntax of project definitions —
`TaskDescription`, `ResourceDescription`, `DurationD`, `DurationAlias`,
`Resource` — independent of any concrete syntax. Three surfaces speak it:

1. `.ug` script statements (`Script.*`),
2. TOML files (`Toml`),
3. the TUI's form fields.

`Lang.Parser` holds the expression-level grammar (durations like
`uniform 1 5` / `normal 13 2` / `logNormal 13 0.5`, plain and quoted
names), polymorphic in the megaparsec error component so the script
parser (custom error type, see below) and standalone parsing (`Void`)
share one grammar. `Lang.Render` is its inverse; the pair is round-trip
tested. Number literals accept both `10` and `10.5`.

Names intern to `Symbolize.Symbol` (`TaskName`, `Resource`,
`DurationAlias` are newtypes over it). `Ord Symbol` is lexicographic
UTF-8, so sorted output is alphabetical; `show` on a Symbol produces
`Symbolize.intern "…"`, so user-facing output must go through `ToText`.

## Sim: simulation and statistics

`Sim.Duration` interprets `DurationD` as a sampler in any
`MonadDistribution` (monad-bayes); the log-normal model follows
Bernhardsson's blow-up-factor analysis (link in source). `Sim.Estimate`
runs N simulations and keeps completion times of runs that scheduled
every task, as weighted `Sim.Stats.Samples`. `Sim.Stats` provides
weighted mean, quantiles, and histograms — this module feeds every
number both frontends display.

## Storage

**TOML (primary).** Spec in `TOML-FORMAT.md`, example in
`resources/example.toml`, codecs (tomland) in `UncertainGantt.Toml`. A
file holds any number of `[[project]]` entries; each has a free-form
`[project.meta]` string table preserved verbatim by tools (the
forward-compatibility slot), and resource/duration/task tables whose
duration values reuse the `Lang` expression syntax as strings. Multiple
projects per file is a first-class concept: tools edit one entry and
re-encode the file preserving the rest.

**`.ug` scripts (legacy).** `Script.Types` defines `Statement` (the
declarative statements plus `print …`/`run simulations` commands);
`Script.Parser`/`Script.Render` are the file syntax. The CLI interprets
statements through a small streaming stack: `StatementInterpreter` (a
class with an associated `Output` type family), `InterpreterState`
(state transitions, using the strict builder), `ConsoleInterpreter`
(adds text output for print commands), `Runner` (file/REPL drivers).
Output is a `Stream (Of Text) IO` rather than direct printing.

A genuinely nice trick lives at the parser/REPL seam: when a `task`
header parses but input ends where the indented block should continue,
the parser emits a custom megaparsec error component
(`MoreInputExpected`), which the REPL uses to switch into multi-line
continuation mode. Parser and REPL agree on "incomplete vs wrong"
through the type system.

## The TUI

Design rationale and sketches: `tui/DESIGN.md`. The important structural
idea is a **two-model split**:

- **`Tui.Doc.Doc`** (`[Element]` — resources, duration aliases, tasks)
  is the editing model. It tolerates every invalid state; all edit
  operations (`DocOp`) are total. `OpReplace` propagates renames
  (resource/alias/task) to referencing tasks so a rename never orphans
  anything. Persistence serializes the `Doc` as-is, so half-broken
  projects save and reload fine.
- **`Project`** is only produced on demand via
  `docProjectIssues :: Doc -> (DocProject, [DocIssue])`, which layers
  doc-level checks (duplicate/unknown aliases) over the tolerant builder.
  `DocIssue` is the single source of validation truth: the estimate pane
  renders the list, `View.taskRows` flags implicated rows with `!`
  (via `issueTasks`), and the detail line shows the selected row's
  issues. Estimates run on the usable subset and report how many tasks
  were excluded.

Module roles: `Tui.Doc` (model, ops, validation, form specs, format
conversions), `Tui.View` (pure screen derivation: dependency-ordered
task rows with depth indentation, panel rows with usage joins, compact
duration notation `1–5d` / `~13d ±2` / `~13d ×1.6`, width-aware table
layout), `Tui.Widgets` (reflex-vty form with per-field completion,
key helpers), `Tui.Estimate` (report building/rendering), `Main`
(FRP wiring, screens as `Reflex.Workflow` steps, persistence dispatch).

Screens: a task table (vocabulary strip on top, detail line below),
`R`/`D` management panels for resources/durations (usage columns,
guarded deletes — deleting something still referenced needs a second
`x`), and add/edit forms with completion (`C-n`/`C-p` cycle candidates;
Depends-on completes per comma-separated segment). Unsaved changes gate
quitting the same way. `Main.AppConfig` isolates persistence: `.ug`
paths load/save scripts, everything else is TOML with project selection
by name (`uncertain-gantt-tui FILE [PROJECT]`).

Two reflex-vty facts that cost real debugging time, encoded in the code:
initial focus must be requested by `FocusId` (`tile'` + `Refocus_Id`) —
a `Refocus_Shift` at post-build samples the focus set before fields
register and silently does nothing; and key events reach widgets only
when their `tile` has focus, which is what scopes form-field completion
keys per field.

## Testing

`test/Spec.hs` is a hand-rolled assertion suite (no framework):
script render/parse round-trips, `Sim.Stats` histogram bucketing, TOML
encode/decode round-trips and defaults, and tolerant-builder semantics
(every issue kind, cascade exclusion, last-wins duplicates). Run with
`cabal test`.

The TUI modules are compiled into the executable only, so they are not
reachable from the test suite; TUI changes are verified by driving the
binary under tmux (see CLAUDE.md).

## Refactoring opportunities

Known rough edges, kept here deliberately (details in
`historical-docs/ARCHITECTURE-REVIEW-2026-07.md` where applicable):

1. **CLI gantt rendering is broken.** `Gantt.renderGantt` returns each
   task's name/legend/padding/bar as four separate list elements and the
   console prints one per line; `print run random` output is unreadable.
   Relatedly, `ConsoleInterpreter.handlePrintGantt` pattern-matches
   `(gantt, Nothing)` and crashes when a simulation cannot schedule every
   task (reachable with `resource X 0`).
2. **The script interpreter stack is heavier than its use.** A typeclass
   with an associated type family (`StatementInterpreter`) exists for
   exactly two instances, one wrapping the other; domain errors travel
   by `throwIO`/`catches` and reach users as `show`n exceptions. If the
   `.ug` format stays, this collapses naturally into one module with a
   function-record seam and `Either`-based errors (reusing the TUI's
   issue rendering); if `.ug` is retired, the whole stack is CLI-only
   legacy.
3. **`.ug` saves can drop tasks.** `Tui.Doc.toStatements` topologically
   sorts tasks to satisfy the interpreter's insertion order and silently
   discards cycle-stuck tasks. TOML saves don't have this problem (task
   order is preserved verbatim); it's one more reason `.ug` is legacy.
4. **`Sim.Stats.quantile`'s interpolation term mixes units** (cumulative
   vs per-sample weight). Numerically negligible at n=1000 but wrong;
   the module needs property tests — both of its historical bugs
   produced plausible-looking numbers.
5. **`Tui.Doc` imports `Tui.Widgets`** (for the `FormField` type),
   which is the only thing keeping the pure TUI modules (`Doc`, `View`)
   inside the executable and out of the test suite's reach. Moving
   `FormField` into a widget-free module (or the library) unlocks unit
   tests for topo-sorting, rename propagation and table layout.
6. **Estimates run synchronously** inside `performEvent`, freezing the
   TUI for the duration of 1000 simulations. `performEventAsync` plus a
   "running…" state is the fix.
7. **`Sim.Duration.estimateAverage`** for log-normal runs a
   10,000-sample Monte Carlo where the closed form
   (median · e^(σ²/2)) is one exact line.
8. **Task-table selection is index-based**; after a re-sort the cursor
   stays at the same row number rather than following the task identity.
9. **Housekeeping:** `release.sh` copies from a hardcoded ghc-8.10.4
   path; `tested-with: ghc ==9.8.2` is wrong (9.12.4 is what's used);
   `license: NONE`; no CI runs the build/tests/formatting.
