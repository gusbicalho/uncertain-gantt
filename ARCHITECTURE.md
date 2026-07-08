# Architecture

uncertain-gantt is a probabilistic project planner: task durations are
probability distributions, and project completion times are estimated by
Monte Carlo simulation under resource and dependency constraints. It
ships three frontends over one library — a CLI that runs `.ug` scripts
(with a REPL), an interactive TUI editor, and a browser-based web editor
(Hyperbole) with the same feature set as the TUI.

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
┌─ Editor model (shared by TUI and web, editor/) ─────────────────────┐
│  Editor.Doc          tolerant editing model + form specs            │
│  Editor.View         pure row derivations + terminal renderers      │
│  Editor.Estimate     Monte Carlo report runner                      │
│  Editor.FormField    UI-agnostic form-field description             │
│  Editor.Persistence  AppConfig loading: .ug vs TOML dispatch        │
└──────────────────────────────────────────────────────────────────────┘
┌─ Frontends ─────────────────────────────────────────────────────────┐
│  app/   CLI: run scripts, REPL (uncertain-gantt)                    │
│  tui/   terminal editor: Tui.{Widgets, EstimateRender}, Main        │
│  web/   browser editor: Web.App, Main (uncertain-gantt-web)         │
└──────────────────────────────────────────────────────────────────────┘
```

Dependency rules, enforced by imports (checkable with grep):

- The domain core imports nothing from `Lang`, `Sim`, `Script`, or `Toml`.
- `Lang` and `Sim` do not import `Script` or `Toml`.
- The editor layer's only `Script` imports are the `.ug` persistence
  boundary (`parseScript`, `renderDeclarations`, `Statement`), all in
  `Editor.Doc`/`Editor.Persistence`; swap those and the editors know
  nothing about scripts.
- `Editor.*` imports nothing from reflex-vty or hyperbole — it is the
  UI-framework-free layer both GUI frontends share (`hs-source-dirs:
  editor` in both executables). `tui/` may import `Editor.*` but not
  `Web.*`, and vice versa.

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

This is what lets frontends pick their own duration type (the
interpreter uses `d = (Maybe DurationAlias, DurationD)` to remember
alias provenance; the TUI uses plain `DurationD`) and what keeps
distributions, parsing and text out of the core. The simulator
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
- **`Project.Tolerant`** (`TolerantBuild r da dd`): Validation-applicative
  style. Declarations — resources, duration aliases, and tasks whose
  durations are `Either` an alias or a direct value — are only
  *collected* (a Writer; order is irrelevant), and `runTolerantBuild`
  validates the whole set at once, resolving alias references and
  returning **all** issues plus a `Project r dd` containing everything
  usable. Tasks are excluded — each with a named `BuildIssue` — for
  undeclared resources, unknown duration aliases, undeclared
  dependencies, cycle membership (SCCs via `Data.Graph`), or transitive
  dependence on an excluded task. Duplicate names keep the last
  declaration and report the shadowing. Used by the TUI.

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

## The editor layer (`editor/`)

Design rationale and sketches: `tui/DESIGN.md` (written for the TUI; the
web frontend inherits the same model). The important structural idea is
a **two-model split**:

- **`Editor.Doc.Doc`** (`[Element]` — resources, duration aliases, tasks)
  is the editing model. It tolerates every invalid state; all edit
  operations (`DocOp`) are total. `OpReplace` propagates renames
  (resource/alias/task) to referencing tasks so a rename never orphans
  anything. Persistence serializes the `Doc` as-is, so half-broken
  projects save and reload fine.
- **`Project`** is only produced on demand via `docProjectIssues`, a
  thin adapter that feeds the whole document (resources, duration
  aliases, tasks) to the tolerant builder. `BuildIssue` is the single
  source of validation truth: the estimate pane renders the list
  (`Editor.Doc.renderIssue`), `View.taskRows` flags implicated rows with
  `!` (via `Tolerant.issueTasks`), and the detail line shows the
  selected row's issues. Estimates run on the usable subset and report
  how many tasks were excluded.

Module roles: `Editor.Doc` (model, ops, validation, form specs, format
conversions), `Editor.View` (pure screen derivation: dependency-ordered
task rows with depth indentation, panel rows with usage joins, compact
duration notation `1–5d` / `~13d ±2` / `~13d ×1.6`, plus the TUI's
width-aware table renderers — the web frontend uses only the row types),
`Editor.Estimate` (Monte Carlo report; structured `Report`, no
rendering), `Editor.FormField` (label/initial/completions triple driving
both frontends' forms), `Editor.Persistence` (`AppConfig`,
`loadAppConfig`: `.ug` vs TOML dispatch by extension, project selection
by name — both frontends take `FILE [PROJECT]` on the command line).

## The TUI (`tui/`)

`Tui.Widgets` (reflex-vty form with per-field completion, key helpers),
`Tui.EstimateRender` (ASCII report/histogram), `Main` (FRP wiring,
screens as `Reflex.Workflow` steps).

Screens: a task table (vocabulary strip on top, detail line below),
`R`/`D` management panels for resources/durations (usage columns,
guarded deletes — deleting something still referenced needs a second
`x`), and add/edit forms with completion (`C-n`/`C-p` cycle candidates;
Depends-on completes per comma-separated segment). Unsaved changes gate
quitting the same way.

Two reflex-vty facts that cost real debugging time, encoded in the code:
initial focus must be requested by `FocusId` (`tile'` + `Refocus_Id`) —
a `Refocus_Shift` at post-build samples the focus set before fields
register and silently does nothing; and key events reach widgets only
when their `tile` has focus, which is what scopes form-field completion
keys per field.

## The web frontend (`web/`)

`uncertain-gantt-web FILE [PROJECT]` serves the same editor at
`http://localhost:3000`, built on Hyperbole (server-rendered HTML over a
websocket; every interaction is an `Action` handled server-side with a
targeted fragment re-render — no client-side app code). Structure:

- **One `HyperView` (`Web.App.App`)** carries the whole UI; a `Screen`
  value in state selects the sub-view (task table + estimate panel,
  resources panel, durations panel, or an add/edit form). This mirrors
  the TUI's workflow-step navigation rather than URL routing — screens
  are not bookmarkable in the TUI either.
- **State is one global `TVar AppState`** (doc, screen, guarded-delete
  armed index, last report, dirty flag, save callback). Hyperbole has no
  server-side session store — its `Session`/`ViewState` mechanisms
  round-trip through cookies/HTML attributes, unsuitable for a whole
  `Doc` — and `update` is dispatched by the library with no way to pass
  a handle in, hence the module-level `TVar` (single-file editor, one
  document per process, so global state is the honest shape).
- **Forms are driven by `FormSpec` generically**: each `FormField`
  renders as a text input named `field-<i>` with a `<datalist>` of its
  completions (the browser-native replacement for the TUI's `C-n`/`C-p`
  cycling); submit collects the fields positionally and calls
  `formParse`, showing its error above the form on failure.
- **Guarded deletes** work like the TUI's: Delete on a referenced
  element arms a `Maybe Int` and the button becomes "Confirm delete?";
  any other action disarms; unreferenced elements delete immediately.
  Rename propagation comes free from `Editor.Doc.applyOp`.
- The estimate panel is always visible next to the task table (no
  split/tab toggle — screen space isn't scarce in a browser) and renders
  the histogram as CSS bars from `Stats.HistogramEntry`.

Deliberate omissions vs the TUI: no quit guard (closing a tab isn't an
app action; the dirty flag is shown in the header instead), no
per-segment completion in Depends-on (datalist matches whole values).

## Testing

`test/Spec.hs` is a hand-rolled assertion suite (no framework):
script render/parse round-trips, `Sim.Stats` histogram bucketing, TOML
encode/decode round-trips and defaults, and tolerant-builder semantics
(every issue kind, cascade exclusion, last-wins duplicates). Run with
`cabal test`.

The `Editor.*`, `Tui.*` and `Web.*` modules are compiled into the
executables only, so they are not reachable from the test suite; TUI
changes are verified by driving the binary under tmux, web changes by
driving the server with a browser (see CLAUDE.md).

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
5. **The `editor/` modules are UI-framework-free but still
   test-unreachable.** The `FormField`-in-`Tui.Widgets` coupling that
   used to force this is gone (`Editor.FormField` is widget-free, and
   `Editor.*` is shared by both GUI executables), but the modules live
   in executable `hs-source-dirs`, not the library, so the test suite
   still can't import them. Moving `editor/` into the library (or a
   sublibrary) would unlock unit tests for topo-sorting, rename
   propagation and table layout.
6. **Estimates run synchronously** in both GUI frontends — inside
   `performEvent` in the TUI (freezing it for the duration of 1000
   simulations; `performEventAsync` plus a "running…" state is the fix)
   and inside the `RunEstimate` handler in the web app (blocking that
   request; Hyperbole's `pushUpdate` from a forked thread is the fix).
7. **`Sim.Duration.estimateAverage`** for log-normal runs a
   10,000-sample Monte Carlo where the closed form
   (median · e^(σ²/2)) is one exact line.
8. **Task-table selection is index-based**; after a re-sort the cursor
   stays at the same row number rather than following the task identity.
9. **Housekeeping:** `release.sh` copies from a hardcoded ghc-8.10.4
   path; `tested-with: ghc ==9.8.2` is wrong (9.12.4 is what's used);
   `license: NONE`; no CI runs the build/tests/formatting.
