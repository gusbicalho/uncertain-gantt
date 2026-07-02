# Architecture review: uncertain-gantt

Scope: the uncertain-gantt library, CLI, TUI, tests and project files as of
commit `aeed053` (2026-07-02). `vendor/tricorder` explicitly excluded.
Review only — no code was changed. Line references are to that commit.

~3,000 lines of Haskell: `src/` (library, ~1,200), `tui/` (~1,350),
`app/` + `test/` (~160).

## System map

```
                    ┌────────────────────────────────────────────┐
                    │ Domain core (polymorphic in r, d)          │
                    │  Task ── Project ── Simulator ── Gantt     │
                    └───────────────▲────────────────────────────┘
                                    │
        ┌───────────────────────────┼────────────────────────────┐
        │ Script layer              │                             │
        │  Types ── Parser ── Duration ── Estimate ── Stats      │
        │  Render (statements → script text)                     │
        │  StatementInterpreter ── InterpreterState              │
        │            └── ConsoleInterpreter ── Runner            │
        └───────▲───────────────────────────────────▲────────────┘
                │                                   │
        app/Main.hs (CLI/REPL)              tui/ (reflex-vty editor)
                                             Doc ── View ── Widgets ── Main
```

Two frontends share the domain core and the script layer's pure parts
(Parser, Render, Estimate, Stats). The CLI additionally uses the
interpreter stack; the TUI deliberately bypasses it and keeps its own
document model (`Tui.Doc`), using scripts only as the persistence format.

## The good

- **The domain core is genuinely polymorphic and small.** `Project r d`,
  `Task r d`, `Simulator.simulate` don't know anything about
  distributions, script syntax, or text — the simulator takes a
  `DurationEstimator d m = d -> m Word` and a `Prioritization` strategy as
  plain functions (`Simulator.hs:31-33`). That is why the TUI could reuse
  simulation with a different duration annotation type without touching it.
- **Validation at construction.** `Project` can only be built/edited
  through `BuildProjectM`; `addTask` checks missing resources, missing
  dependencies and dependency cycles at insert time (`Project.hs:86-113`).
  The invariant "a stored project has no dangling references and no
  cycles" holds everywhere downstream.
- **The simulator reports partial failure honestly**: it returns
  `(Gantt, Maybe unscheduledTasks)` instead of pretending every run
  completes (`Simulator.hs:40`) — the caller decides what an incomplete
  schedule means (Estimate filters those runs out).
- **Streaming output as the interpreter interface.** Handlers emit
  `Stream (Of Text) IO` rather than calling `putStrLn`
  (`ConsoleInterpreter.hs`), so output is composable and testable in
  principle, and the Runner just `S.mapM Text.IO.putStrLn`.
- **`Estimate` and `Render` extracted as shared, pure-ish seams** (the
  recent refactor): CLI and TUI now run the same Monte Carlo path
  (`Script/Estimate.hs`) and the same statement rendering, and the
  render/parse pair is round-trip tested (`test/Spec.hs`).
- **The TUI keeps the model pure.** `Tui.Doc` (ordered element list, pure
  ops, pure validation) and `Tui.View` (topo sort, notation, table
  layout) contain no reflex; the FRP wiring is confined to `Main`/`Widgets`.
  Rename propagation lives in the model (`Doc.applyOp`), not the UI.
- **Consistent style discipline**: fourmolu config, aggressive warning set
  (`-Wall -Wincomplete-uni-patterns -Wmissing-export-lists ...` in the
  cabal common stanza), `ImportQualifiedPost` everywhere, deriving
  strategies explicit.

## The bad (concrete defects found while reviewing)

1. **`renderGantt` emits each task as four separate lines** — name,
   resource+legend, leading spaces, and the bar are returned as separate
   list elements (`Gantt.hs:64-69`) and the consumer prints one per line
   (`ConsoleInterpreter.hs:128`, `Runner`). Verified against the built
   CLI: `print run random` output is unreadable. The four fragments are
   clearly meant to be concatenated into one row. Likely broken in the
   streaming refactor when `printGantt`'s direct `putStr` calls became a
   `[Text]`.
2. **`handlePrintGantt` crashes on incomplete simulations.** The partial
   pattern `(gantt, Nothing) <- lift . Sampler.sampleIO $ ...`
   (`ConsoleInterpreter.hs:122`) calls `MonadFail` if any task couldn't be
   scheduled — trivially reachable with `resource Dev 0`. Should degrade
   like the estimate path does.
3. **`Stats.quantile`'s interpolation term mixes units.** In
   `v + ((nextV - v) * (targetW - w) / (nextW - w))` (`Stats.hs:56`), `w`
   is a *cumulative* weight while `nextW` is a *single sample's* weight,
   so the fraction is dimensionally wrong. With many samples the term is
   numerically tiny (both numerator and denominator are negative, ratio
   ≈ 0), which is why outputs look plausible; with few samples it's
   visibly off. This module has no direct tests beyond the recently added
   histogram checks — the histogram label bug fixed this week
   (`lowerEndFirst + i*(bucketSize-1)`) lived here undetected for years,
   which is evidence the module needs property tests, not spot checks.
4. **Docs describe a different language.** Both `ARCHITECTURE.md` (block
   syntax: `task "..." { need Dev = 1; duration = normal(10, 2) }`) and
   `WALKTHROUGH.md` (`duration short = uniform 1 3`, inline
   `task "Design API" Dev medium`, `run 1000`) show syntax the parser has
   never accepted. Anyone learning the DSL from the docs will write
   scripts that don't parse; `resources/example.ug` is the only accurate
   reference.
5. **`release.sh` is dead**: it copies from a hardcoded
   `ghc-8.10.4` build path; the project builds with 9.12.4 (and
   `tested-with: ghc ==9.8.2` in the cabal file is a third, also-wrong
   claim). The CLI usage text is also stale — it says `<script-file> -i`
   is "not yet implemented in streaming mode" while `app/Main.hs:32-38`
   implements exactly that.
6. **Errors reach users as `show`n exceptions.** `Output
   InterpreterState = SomeException`, and the console layer prints
   `showText ex` (`ConsoleInterpreter.hs:60-64`), so a typo'd alias
   surfaces as `user error (Unknown duration alias foo)`. The TUI's pure
   validation (`Doc.docProject`) produces human sentences for the same
   conditions — the quality gap is entirely in the plumbing.

## The unusual

- **Exceptions as the interpreter's error channel.** `InterpreterState`
  handlers run in `IO` and `throwIO` domain errors (`userError` for
  unknown aliases, `BuildProjectError` as an `Exception` instance), which
  `interpretStmt` immediately catches and re-emits as stream elements
  (`InterpreterState.hs:60-72`). It works, and it keeps handler signatures
  simple, but it means `Either`-shaped domain logic does a round-trip
  through the RTS exception machinery, and only the two anticipated
  exception types are caught — anything else escapes the stream contract.
- **A 4-deep `StateT` stack navigated by hand-rolled lifts.** The
  simulator threads gantt/done/todo/resources as four nested `StateT`
  layers, with `liftResources = lift . lift . lift` etc.
  (`Simulator.hs:85-89`). It's honest about what state exists, and the
  aliases make the intent readable, but adding one more piece of state
  renumbers everything. A single record state (or named effects) would be
  more conventional.
- **String interning for all names.** `TaskName`/`Resource`/
  `DurationAlias` wrap `Symbolize.Symbol` — a global intern table — rather
  than `Text`. For workloads this size the win is theoretical, and it
  costs a third-party dependency plus non-obvious semantics (checked:
  `Ord Symbol` *is* lexicographic UTF-8, so sorting is fine). The `Show`
  instance leaks `Symbolize.intern "..."` into any `show`-based output —
  which is exactly what the old demo TUI accidentally displayed.
- **The interpreter abstraction has exactly two instances**, and one wraps
  the other. `StatementInterpreter` (class + associated `Output` type
  family) exists to let `ConsoleInterpreter` decorate `InterpreterState`
  (`StatementInterpreter.hs`). It's a lot of machinery for a
  decorator; a plain function record would do the same job. (The TUI, the
  obvious third client, chose not to use it at all.)
- **The TUI's model module imports a widget module.** `Tui.Doc` depends on
  `Tui.Widgets` for the `FormField` type, inverting the otherwise clean
  model→view layering. Deliberate tradeoff (kept `FormSpec`
  self-contained), but it means `Doc` can't move into the library, which
  in turn blocks unit-testing `Doc`/`View` from the test suite (they're
  compiled into the executable only).

## The interesting

- **REPL continuation via a custom megaparsec error component.**
  `MoreInputExpected` rides inside the parser's error type; when a `task`
  header parses but EOF arrives where an indented block should be,
  `onEOFExpect` converts the failure into a fancy error carrying
  `ExpectedMultilineInput` (`Parser.hs:212-218`), which the REPL uses to
  switch into `|   ` continuation-prompt mode (`Runner.hs:79-80`). Parser
  and REPL agree on "incomplete vs wrong" through the type system. Nice.
- **Cycle checking exploits an insertion invariant.** `addTask` only runs
  the (expensive) cycle check when the task name *already exists*
  (`Project.hs:103-113`) — a brand-new task can't create a cycle because
  its dependencies must already exist and can't point forward to it.
  Subtle and correct; deserves a comment it doesn't have, and it makes
  order matter in ways the TUI's Doc had to re-solve with its own
  topological sort (`Doc.sortTasks`, ignoring unknown deps instead).
- **Knot-tied transitive closure.** `transitiveDependents` defines a lazy
  map whose values reference the map itself (`Project.hs:115-135`) —
  memoized DAG traversal in five lines. Elegant, and safe *only* because
  of the no-cycles invariant above; on a cyclic graph it would loop
  forever. The two facts are load-bearing for each other and live 20
  lines apart with no cross-reference.
- **The log-normal estimator** is grounded in a real model of software
  estimation (Erik Bernhardsson's blow-up-factor post, linked in the
  source) rather than being an arbitrary third distribution.
- **Time-skipping simulation loop**: rather than ticking `t+1`, the
  scheduler jumps to the next task-completion time (`nextRelevantT`,
  `Simulator.hs:65-69`).

## Complexity hotspots

Ranked by (how hard to modify safely) × (how likely to need modification):

1. **`tui/Main.hs` `runApp` + `editorPane`** — one large `rec` block wiring
   doc state, selection, estimate, save, quit-guard, status line, and a
   workflow of five step kinds that communicate through a single
   `EditorMsg` sum. Each feature added (dirty tracking, quit arming,
   delete arming, status routing) has grown the knot. It works, but the
   next feature (async estimates, undo) will fight it. The `EMsg*`
   routing where every step re-derives `fforMaybe` projections is
   boilerplate that will scale linearly with message kinds.
2. **`Simulator.simulate`** — the 4-layer transformer stack plus
   scheduling logic (completion processing, resource take/release, time
   skipping) in one function. Correctness currently rests on careful
   reading; there are no simulator-specific tests at all.
3. **`Stats.hs`** — small but subtle (weighted samples, quantile
   interpolation, bucket math), historically buggy, and feeding every
   number both frontends display.
4. **`Parser.taskDescription`** — the two-space-indent block grammar with
   optional dependency/description lines and the EOF-continuation trick
   is the densest part of the parser; changing the surface syntax (which
   "we may move away from .ug anyway" implies) lands here.

## Opportunities for improvement

Ordered roughly by value-for-effort:

1. **Fix the two verified CLI bugs**: join `renderGantt`'s fragments into
   one line per task, and make `handlePrintGantt` handle incomplete
   simulations gracefully. Both are small and user-visible.
2. **Property tests for `Stats`** (quantile monotonicity, quantile of
   uniform weights ≈ order statistic, histogram fractions sum to 1,
   bucket lower ends partition the range) and a golden test for CLI
   output of a fixed-seed run. The render/parse round-trip test is
   example-based but property-shaped — QuickCheck over generated
   `[Statement]` would generalize it cheaply.
3. **Rewrite the stale docs** (`ARCHITECTURE.md` DSL sample,
   `WALKTHROUGH.md` syntax, README's captured `Up to date` noise, CLI
   usage text) or delete them in favor of `resources/example.ug` +
   `tui/DESIGN.md`, which are accurate. Wrong docs are worse than none.
4. **Unify validation.** `Doc.docProject` (pure, friendly messages) and
   `BuildProjectM` + IO exceptions (interpreter path) check the same
   things with different error quality. Making `BuildProjectError`
   rendering human-readable and threading `Either` through the
   interpreter (dropping the throw/catch round-trip) would let the TUI's
   `prettyBuildError` be *the* error renderer.
5. **Extract `FormField` out of `Tui.Widgets`** (it's pure data) so
   `Tui.Doc`/`Tui.View` lose their reflex-adjacent import, can move into
   the library (or a sublibrary), and become reachable from the test
   suite. `View.taskRows`' topo/depth logic and `Doc.applyOp`'s rename
   propagation are exactly the kind of pure logic that should have unit
   tests and currently can't.
6. **Async estimates in the TUI.** `runReport` runs 1000 simulations
   inside `performEvent`, freezing the UI for the duration. reflex's
   `performEventAsync` plus a "running..." state in the estimate pane
   would fix the only notable interaction stall.
7. **Decide the fate of the interpreter abstraction.** If the script layer
   stays, collapse `StatementInterpreter`/`InterpreterState`/
   `ConsoleInterpreter` into one module with a function-record seam. If
   the `.ug` format is retired as the primary interface (per DESIGN.md),
   the whole `Runner`/interpreter stack becomes CLI-only legacy and could
   move under an explicitly-named `Legacy`/`Cli` namespace to stop new
   code from depending on it.
8. **Housekeeping**: fix or delete `release.sh`; correct `tested-with`;
   pick a license (`license: NONE` contradicts having public release
   packaging); add CI (there is none — the strong warning flags only help
   if something runs them); consider replacing the `allow-newer: base`
   blanket with a targeted relaxation.
9. **`Duration.estimateAverage` for log-normal** runs a 10,000-sample
   Monte Carlo (with an `error` call on the impossible-empty case) every
   time it's asked for an average — the median×`exp(σ²/2)` closed form
   is one line and exact.

## Overall assessment

The codebase is in good shape where it matters: the domain core is small,
principled and reusable, and the recent TUI work demonstrated that the
architecture actually delivers on its promise — a second frontend was
built without modifying the simulator or domain model, and the seams that
were missing (`Estimate`, `Render`) extracted cleanly. The script
interpreter stack is the weakest area: it carries abstraction weight
disproportionate to its two instances, uses exceptions where values would
do, and owns both verified user-facing bugs. The second-weakest area is
verification — the test suite is four assertions and there is no CI, which
is thin for a codebase whose core outputs are *statistics* (where wrong
numbers look plausible; both Stats bugs found this week printed
convincing-looking output for years).
