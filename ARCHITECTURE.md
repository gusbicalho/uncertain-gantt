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
│  web/   browser editor: Web.{App, Route, State, Editor, Files,      │
│         Styles}, Main (uncertain-gantt-web)                         │
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

Module roles: `Editor.Doc` (model, ops, validation, form specs — strict
variants used by the TUI plus lenient ones used by the web editor, where
an empty task resource/duration commits as an empty-named reference the
tolerant builder then flags — and format conversions), `Editor.View`
(pure screen derivation: dependency-ordered
task rows with depth indentation, panel rows with usage joins, compact
duration notation `1–5d` / `~13d ±2` / `~13d ×1.6`, plus the TUI's
width-aware table renderers — the web frontend uses only the row types),
`Editor.Estimate` (Monte Carlo report; structured `Report`, no
rendering), `Editor.FormField` (label/initial/completions triple driving
both frontends' forms), `Editor.Persistence` (`.ug` vs TOML dispatch by
extension, project selection by name).

`Editor.Persistence` has two layers. `loadDocument`/`saveDocument`
report failures as `Either Text`, for the web editor, which serves many
documents and must not exit because one of them is malformed;
`loadAppConfig` wraps them into an `AppConfig` that still `die`s on
load failure, for the TUI, which opens one document up front.
`listProjectFiles` enumerates a directory for the web file browser.
**`saveDocument` re-reads the file** rather than closing over the
entries it loaded, and splices the edited entry into the current
contents (`Editor.Doc.toProjectEntry` carries the name and `meta`
across). That is what lets two projects from the same TOML file be
open at once without the second save clobbering the first.

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

`uncertain-gantt-web [--port PORT] [PATH [PROJECT]]` serves the editor
at `http://localhost:3000` (or the given port), built on Hyperbole
(server-rendered HTML; interactions are `Action`s handled server-side
with targeted fragment re-renders — no client-side app code). `PATH` is
a **directory** to browse or a single file to open; either way the
server holds any number of documents open at once. Unlike the TUI's
modal workflow, everything lives on one screen and edits happen in
place (design rationale: `web/DESIGN.md`). Structure:

- **The URL says which document.** `Web.Route` defines
  `/` (redirect to the launch document, or to the browser), `/files`
  (pick a file), and `/edit/<file>[/<project>]`. A document is keyed by
  `DocKey` = file name + project entry. Hyperbole actions POST to the
  current URL and both transports derive `request.path` from WAI's
  `rawPathInfo`, so `Web.State.currentKey` can recover the document
  inside every `update` — which is what makes multi-document work
  without threading a handle through `HyperView` instances, something
  the library gives no way to do. `rawPathInfo` is *not* percent-decoded
  and `routePath` does not encode, so `Web.Route` escapes segments on
  both sides and they round-trip exactly. `/edit/<file>` redirects to
  the project it resolved to, so a file and its first project are one
  open document rather than two.
- **Four `HyperView`s** (`FileBar`, `Header`, `TaskTable`,
  `EstimatePanel`), so each region re-renders independently.
  Doc-changing table actions `trigger` refreshes of the others —
  `trigger`, not `pushUpdateTo`, because form submissions arrive over
  HTTP where pushes are silently dropped but triggers ride back as
  response metadata. The view ids stay nullary: one page shows one
  document, so the URL disambiguates them.
- **State follows Ports-and-Adapters/capability discipline, split across
  three modules.** `Web.Docs` holds plain domain data — `DocState`
  (a document's doc, undo stack, epoch, editing target, reports, dirty
  flag) and `ServerState` (served directory plus a `Map DocKey
  DocState`) — with ordinary field selectors, since both are read
  pervasively throughout `Web.Editor`/`Web.Files`'s view-rendering code.
  `Web.Capability` defines `DocsSurface`/`DocHandle` as pure interfaces
  (records of functions, one effect-row type param) with no `TVar`/`IO`
  anywhere and constructors exported — per Fowler's Separated Interface,
  the interface belongs with the core, not any one adapter, so a
  different implementation (a test mock, say) could satisfy the same
  types without pulling in persistence at all. Both records use
  `NoFieldSelectors`, so every method is dot-only
  (`OverloadedRecordDot`) — there is no prefix-function fallback,
  matching how `Web.Hyperbole`'s own `Request` is read via `req.path`.
  `Web.State` is the one concrete adapter: the `TVar ServerState` is the
  Resource, built once in `newAdapters` and never leaving that module;
  `DocsSurface` is the Surface over it (wide — names any file, any open
  document; shallow — hands out reads and `DocHandle`s, does no
  rendering); `DocHandle` is the Capability it mints, fixed to a single
  `DocKey`, so a handler holding one can't reach a sibling document by
  constructing the wrong key — its methods need only `IOE`, since the
  `TVar` they close over was already read out once, at mint time.
  `Adapters` is the one record of adapters (today, just
  `docs :: DocsSurface`), built once in `Main` and threaded to every
  handler as a single `Reader Adapters` effect (`runReader` composes
  fine around `liveApp` since `HyperView`'s `update` is polymorphic in
  the effect row, it just isn't given a handle as an argument — that row
  is the only channel in). `Adapters`'s field is universally quantified
  over the effect row (`RankNTypes`) because different `HyperView`
  dispatches run in different concrete rows — each adds its own
  `Reader`/`State` layer per Hyperbole's own dispatch mechanism — so the
  one value built in `Main` has to serve all of them, not just the row
  it happened to be built in. `Adapters`'s own constructor isn't
  exported, so holding `Reader Adapters :> es` never means holding the
  raw `TVar`: only `Web.State`'s chosen operations are reachable, not
  unrestricted read/write over every open document. An action against a
  document that isn't open loads it from disk, which is what lets a tab
  left open across a close or a restart keep working (at the cost of
  that document's undo history).
- **Most handlers hold a `DocHandle`, not a bare `DocKey`.**
  `Web.State.requireDocHandle` mints one from the current URL via
  `DocsSurface`'s `docsOpen`. The one legitimate exception is the file
  strip (`Web.Files`), which acts on whichever document's close button
  was clicked; it goes through two narrow, single-purpose surface
  methods (`docsArmClose`, `docsClose`) rather than a capability
  exposing an arbitrary mutator over an arbitrary key.
- **Path confinement:** a file name from a URL only resolves if it
  appears in the served directory's own listing (`Web.State.isServedFile`),
  so a crafted `/edit/..%2F..%2Fetc%2Fpasswd` is a 404.
- **The file strip** (`FileBar`, on every page) lists open documents with
  dirty markers and a `×` each; closing a dirty one takes two clicks,
  since closing drops its undo stack. The header gains a project picker
  when the file holds more than one project — plain links, because each
  project is its own document at its own URL.
- **Rows edit in place.** Clicking any cell swaps the row for a form
  (fields from `FormSpec`, `<datalist>` completions, the clicked field
  autofocused); Enter commits via `formParse`, Escape cancels. The last
  row of each section is a permanent quick-add form. Task and resource
  forms use the lenient `Editor.Doc` specs, so a bare task name commits
  and the missing pieces surface as row issues ("sketch first").
- **Vocabulary expands in place** (collapsible resources/durations
  sections above the task table), and undefined names offer
  create-from-use quick fixes on the issue line ("Define TeamD ×1" /
  "Define huge…" prefilled in the durations quick-add), driven by
  `taskRowUndefinedResource`/`Duration` from `Editor.View`.
- **The estimate is live.** Committing any change marks the state stale;
  the stale+auto panel renders an `onLoad Recalc` element, so the client
  immediately requests a recalculation. `Recalc` blocks on the
  simulation in its own handler thread — a newer action on the same view
  cancels it server-side (`Concurrency = Replace`; the cancellation map
  is per client, so tabs on other documents are unaffected), so edit
  bursts just restart the run — and an epoch counter discards results that raced
  with an edit; the still-stale render then re-arms the loop. The report
  shows deltas against the previous run (`mean 81.2 +3.7`,
  `p50 76 → 80`). An "auto" toggle falls back to the manual
  Run-estimate button with a stale-report note.
- **Deletes are one click + undo** (a `Doc`-snapshot stack, capped at
  100; the header shows Undo and a "Deleted … — Undo to restore" status)
  instead of the TUI's second-`x` confirmation — undo covers more than
  the guard did. Rename propagation comes free from `Editor.Doc.applyOp`.

Deliberate omissions vs the TUI: no quit guard (closing a tab isn't an
app action; the dirty flag is shown in the header and the file strip
instead), no per-segment completion in Depends-on (datalist matches
whole values). No global keyboard shortcuts (Hyperbole key events
dispatch off the focused element only), so undo is button-only. Two
tabs on the *same* document do not sync — Hyperbole has no cross-client
push here — and creating or renaming files and projects is not
supported from the web UI. The directory scan is flat.

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
6. **Estimates run synchronously in the TUI** — inside `performEvent`,
   freezing it for the duration of 1000 simulations; `performEventAsync`
   plus a "running…" state is the fix. The web app already runs them
   without blocking the UI (each `Recalc` action gets its own handler
   thread; see the web frontend section).
7. **`Sim.Duration.estimateAverage`** for log-normal runs a
   10,000-sample Monte Carlo where the closed form
   (median · e^(σ²/2)) is one exact line.
8. **Task-table selection is index-based**; after a re-sort the cursor
   stays at the same row number rather than following the task identity.
9. **Housekeeping:** `release.sh` copies from a hardcoded ghc-8.10.4
   path; `tested-with: ghc ==9.8.2` is wrong (9.12.4 is what's used);
   `license: NONE`; no CI runs the build/tests/formatting.
