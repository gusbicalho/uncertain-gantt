# Session notes

## 1. Built the main project

`cabal build` succeeded — `uncertain-gantt` library and `exe:uncertain-gantt`, pulling in
`monad-bayes` (pinned to a git commit in `cabal.project`), `brick`, `vty`, `statistics`, etc.

## 2. Installed and set up tricorder

`tricorder` wasn't preinstalled, so it was built from the vendored `vendor/tricorder`
submodule.

- `vendor/tricorder/cabal.project` includes `atelier-testing`, which pins an unreleased
  `tmp-postgres` version range and fails dependency resolution. It isn't a dependency of the
  `tricorder`/`tricorder-daemon` executables, so added
  `vendor/tricorder/cabal.project.tricorder-only` (same as `cabal.project` minus
  `atelier-testing`) and built against that instead:
  ```
  cabal build --project-file=cabal.project.tricorder-only tricorder:exe:tricorder tricorder:exe:tricorder-daemon
  ```
- Built binaries symlinked into `~/.local/bin/tricorder` and `~/.local/bin/tricorder-daemon`
  (already on `PATH`).
- Daemon started from the main project root (`tricorder start`); confirmed clean build (0
  diagnostics) and passing test suite via `tricorder status --wait`.

Note: tricorder's daemon runs a persistent `cabal repl` (GHCi) session under the hood, which
is fast for incremental changes but can hit `GHCi session failed to start: StartupFailed`
when a *fresh* dependency needs heavy compilation at `-O1` (this project's `cabal.project`
has no optimization override, unlike tricorder's own). Workaround used throughout: run a
plain `cabal build` in the background first to warm `dist-newstyle`, then let tricorder pick
up the already-built artifacts.

## 3. Added `reflex-vty` and a new TUI executable

Added to `uncertain-gantt.cabal`:

- New `executable uncertain-gantt-tui` depending on `reflex`, `reflex-vty`, `vty`, `time`,
  `containers`, `text`, `uncertain-gantt`.
- `app/TuiMain.hs`: builds a small hardcoded demo `Project` (2 resources, 4 tasks with
  dependencies), runs it through the existing `UncertainGantt.simulate`/`mostDependentsFirst`
  with plain `Word` durations (deterministic — no probability distributions needed since the
  public API only re-exports `Task`'s raw duration field, not the distribution machinery),
  then animates a day-by-day playback of the resulting Gantt schedule as ASCII progress bars
  using `reflex-vty`'s FRP ticks. `q`/`Esc`/`Ctrl+C` to quit.

Bugs hit and fixed while wiring it up:

1. `scrollableText` wants `Dynamic t Text`; originally passed `current dayDyn` (a
   `Behavior`) — fixed to just `dayDyn`.
2. `col`/`grout` need `HasLayout t m`, which bare `mainWidget` doesn't provide — needed to
   wrap in `initManager_` (same pattern reflex-vty's own examples use).
3. Minor `-Wname-shadowing` (`now` shadowed a `Reflex.Class` export) — renamed to
   `startTime`.

Verified: `cabal build exe:uncertain-gantt-tui` compiles warning-free; `tricorder status
--wait` shows 0 diagnostics across all 4 targets; ran the binary under a pseudo-tty (`script`
+ `TERM=xterm-256color`) — it initializes vty (alternate screen, mouse tracking, cursor
hiding) and tears down cleanly, no exceptions from our code.

This is a starting skeleton wired to the real domain model (`Project`/`Task`/`Simulator`/
`Gantt`), not a placeholder — intended as a base to build further on (e.g. loading a real
`.ug` script instead of the hardcoded project, wiring in the probabilistic simulation,
adding interactivity).

## 4. What `tricorder` actually gives you

Not separate registered tools — one CLI (`tricorder`) invoked via `Bash`, documented by its
skill file. Commands: `start`/`stop`/`restart`, `status` (`--wait`, `--json`, `--verbose`,
`--expand N`), `test-results` (`--failed`, `--wait`), `source MODULE...`, `ui`, `log`.

Its diagnostics engine really is backed by a persistent GHCi session (`cabal repl`), but the
CLI surface doesn't expose GHCi's introspection power — no go-to-definition, hover/
type-at-point, autocomplete, or rename. It's positioning itself as a `ghcid`/`ghciwatch`
replacement (continuous build + diagnostics + test results shared across clients via a
daemon), not `haskell-language-server`. Its `proposals/` dir (RFC-style, 000–007) suggests
active, still-foundational development.

`tricorder source MODULE` only works for *installed dependency* modules, not local project
files (for those, just `Read` the file directly — no lookup problem to solve).

## 5. Fixed two real bugs in `tricorder source` (in the vendored submodule)

File: `vendor/tricorder/tricorder/src/Tricorder/Effects/GhcPkg.hs`

**Bug 1 — dependency modules invisible to source lookup.** `tricorder source Reflex.Vty`
returned `Not found`, while `tricorder source Data.Map.Strict` worked. Root cause: the
`ghc-pkg find-module`/`ghc-pkg field` subprocess calls had no `--package-db` flag, so they
only searched GHC's global/user package DB (where boot libraries like `containers` live).
Regular Hackage dependencies resolved by `cabal build`/`cabal repl` (e.g. `reflex-vty`)
live in the project's per-GHC-ABI cabal store (`~/.cabal/store/ghc-<ver>-<abi>/package.db`),
which plain `ghc-pkg` never sees.

Fix: added `storePackageDbArgs`, which runs `cabal path --output-format=key-value
--compiler-info` (respects the calling process's cwd and any project-specific `store-dir`
override), parses out `compiler-store-path:`, and passes
`--global --user --package-db=<store>/package.db` to both `ghc-pkg` calls. Both `--global`
and `--user` have to be restated explicitly — supplying *any* `--package-db` to `ghc-pkg`
silently drops its implicit default search.

**Bug 2 — surfaced by fixing bug 1.** Once the store DB was searched, ambiguous matches (the
same package version registered under two different unit-id hashes — here, artifacts from an
earlier scratch-project exploration build plus the main-project build) come back from
`ghc-pkg find-module --simple-output` as multiple package IDs space-separated *on one line*.
`findModule` was parsing by splitting on newlines and taking the whole line as one
`PackageId`, so it produced a garbled double-name string. `getHaddockHtml` already handled
this correctly (`T.words`) for `ghc-pkg field`'s output — `findModule` just hadn't matched
it. Fixed to split on whitespace and take the first match, consistent with the design doc's
stated intent (`proposals/003-source-lookup/design.md`, Open Question 1).

Verified after each fix via `tricorder restart` + `tricorder source Reflex.Vty` /
`tricorder source Data.Map.Strict` (regression check) + `tricorder status --wait`.
Existing unit tests (`test/Unit/Tricorder/GhcPkgSpec.hs`) only exercise the scripted test
interpreter, not this real IO path, so unaffected.

Not upstreamed — fixed directly in the vendored submodule per instruction.

## 6. Remaining limitation (not a bug, a build-config tradeoff)

`tricorder source` reads Haddock's `--hyperlinked-source` HTML output
(`<haddock-html>/src/<Module>.html`, tags stripped), not the raw `.hs` files. `reflex-vty`
was built without `--enable-documentation`, so there's no HTML to read —
`tricorder source Reflex.Vty` now correctly resolves the module but reports
`SourceNoHaddock` rather than a false "not found." (`containers` works out of the box because
GHC boot libraries ship with prebuilt docs alongside the toolchain.)

Confirmed via `cabal build --enable-documentation --haddock-hyperlink-source --dry-run` that
turning this on would require rebuilding **148 packages** — essentially the entire
transitive closure of `reflex-vty`, not just the package itself. This is because cabal's
local store is content-addressed by build configuration; flipping the documentation flag
changes every affected package's unit-ID hash, invalidating the existing non-doc build
artifacts. Not done in this session — left as a follow-up if full source-text access to
`reflex-vty` internals is needed later.

## 7. Interactive TUI project editor (replaces the demo TUI)

`app/TuiMain.hs` (day-by-day playback demo) was deleted; `exe:uncertain-gantt-tui` now
lives in `tui/` and is a ground-up interactive editor, not a text editor for `.ug` scripts:

- `tui/Tui/Doc.hs` — the editable document: ordered list of elements (resources, duration
  aliases, tasks), pure ops (insert/replace/delete), validation + conversion to the domain
  `Project` (`docProject`), and `FormSpec`s describing add/edit forms per element type.
  Scripts are only the persistence format (`fromStatements`/`toStatements`; print/run
  statements are dropped with a warning in the status bar).
- `tui/Tui/Widgets.hs` — reusable reflex-vty widgets: `keyEv`, `selectList`, `form`.
- `tui/Tui/Estimate.hs` — runs `UncertainGantt.Script.Estimate.completionSamples` (Monte
  Carlo, 1000 runs) and renders mean/percentiles/histogram.
- `tui/Main.hs` — app wiring: element list + estimate pane (split view, `v` toggles to
  tabbed view, Tab switches tabs), j/k/arrows to move, Enter edit, t/r/u add
  task/resource/alias, x delete, C-r/F5 re-estimate, C-s save, C-q/q quit (guarded:
  unsaved changes require a second quit; C-c force-quits).

Library changes supporting it: `Script.Render` (render statements back to script syntax,
round-trip tested in `test/Spec.hs`), `Script.Estimate` (extracted from InterpreterState so
CLI and TUI share the simulation path), `parseDurationDescription` exported from Parser.

Bugs found by driving the TUI under tmux and fixed:

1. **Form initial focus** — `requestFocus (Refocus_Shift 1)` at postBuild does nothing:
   `runFocus` samples the focus-set *behavior*, still empty at postBuild time. First
   keystrokes were silently dropped until a Tab. Fixed by capturing the first field's
   `FocusId` via `tile'` and requesting `Refocus_Id` instead.
2. **Pre-existing `Stats.histogram` label bug** — `lowerEndForIndex` computed
   `lowerEndFirst + i * (bucketSize - 1)` instead of `lowerEndFirst + (i-1) * bucketSize`,
   so every histogram (CLI too) showed nonsense bucket labels. Also clamped
   `indexForSample` so a sample exactly on the range's upper edge isn't silently dropped.
   Regression test added.
3. **Parser rejected integer durations** (`normal 10 2` failed; megaparsec `Lexer.float`
   demands a decimal point). Parser now accepts both via `try float <|> decimal`.
4. Cosmetic: `-Infinity` underflow bucket renders as "below"; long list lines truncate.

## 9. Task-centric main screen redesign (tui/DESIGN.md)

Implemented the redesign from tui/DESIGN.md: the editor no longer renders .ug-like lines.

- `tui/Tui/View.hs` (new, pure): task rows in topo order with dependency-depth
  computation (`taskRows`), resource/duration panel rows with used-by joins, compact
  duration notation (`1–5d`, `~13d ±2`, `~13d ×1.6` = e^σ), vocabulary strip with
  `+n more` truncation, and a shared column-table renderer (first column clamped at 28,
  others 14, last column takes the rest, `…` truncation, scroll-follows-selection).
- Main screen: vocabulary strip (2 lines) + task table (columns TASK/RESOURCE/DURATION/
  AFTER, `≡` = has description, `!` = broken reference or cycle) + detail line showing
  the selected task's description.
- `R`/`D` open resource/duration management panels (workflow steps like forms):
  usage-aware rows, full info for the selected row in a 2-line detail area, `a` add,
  Enter edit, `x` delete with a second-`x` guard when the element is referenced
  (same for tasks that others depend on), Esc back.
- `OpReplace` now propagates renames (resource/alias/task) to referencing tasks.
- Keys changed: `a`/`t` add task, `R` resources, `D`/`u` durations; selection index
  ranges over task rows only. New `EMsgStatus` lets steps put warnings in the status bar.
- Verified end-to-end under tmux: topo/indent rendering, guards, rename propagation
  (TeamB→TeamZ and task Discovery→Kickoff updated all referents), `!` flags after
  force-deleting used vocabulary, estimate/save/reload, CLI parses the saved file.

## 10. Namespace restructure: Lang / Sim / ToText extracted from Script

The modules shared by the TUI and the script interpreter moved out of
`UncertainGantt.Script.*`:

- `UncertainGantt.Lang.Types` — the definition vocabulary (Resource, DurationAlias,
  DurationD, TaskDescription, ResourceDescription), extracted from Script.Types.
- `UncertainGantt.Lang.Parser` — expression-level parsers (duration, names,
  parseDurationDescription), extracted from Script.Parser and made polymorphic in the
  megaparsec error component (`Ord e => Parsec e String a`) so Script.Parser instantiates
  them at MoreInputExpected and standalone use instantiates at Void.
- `UncertainGantt.Lang.Render` — renderDuration/renderName, extracted from Script.Render.
- `UncertainGantt.Sim.{Duration,Estimate,Stats}` — moved from Script.* (simulation and
  statistics, not scripting).
- `UncertainGantt.ToText` — moved to the top level; fixes the inversion where core modules
  (Task, Gantt) imported from Script.*.

`UncertainGantt.Script.*` now holds only the script format: Statement/PrintGanttType/
MoreInputExpected (Types), statement-level Parser/Render, and the interpreter stack.
The TUI's only Script imports are the persistence boundary: parseScript,
renderDeclarations, and the Statement constructors used by fromStatements/toStatements.
Core, Lang and Sim import nothing from Script (verified by grep).

## 11. TOML project file format (TOML-FORMAT.md)

New storage format for projects, parsed/written with tomland (1.3.3.3, builds fine on
GHC 9.12 thanks to the existing `allow-newer: base`). Spec in TOML-FORMAT.md; example in
resources/example.toml.

- A file holds `[[project]]` entries: name + optional free-form `[project.meta]` string
  table (preserved verbatim by tools) + `[[project.resource]]`/`[[project.duration]]`/
  `[[project.task]]` arrays. Duration values reuse the Lang expression syntax as strings
  ("uniform 1 5" or an alias name) — same surface syntax as TUI form fields and .ug.
- `UncertainGantt.Toml`: ProjectsFile/ProjectEntry + tomland codecs + decode/encode.
  tomland specifics: `Toml.list` and `Toml.tableMap` decode missing keys to empty;
  optional scalar keys use a `withDefault` combinator (dioptional + dimap) that also
  omits the key when the value equals the default.
- TUI: extension dispatch in `start` (.ug → legacy script path, else TOML), new
  `AppConfig` record (title/initial doc/note/save function) so runApp is
  persistence-agnostic. `uncertain-gantt-tui FILE [PROJECT]` opens a named project;
  unknown name dies listing available ones; missing file starts a new single-project
  file. Save re-encodes the whole file replacing only the edited entry (by load index),
  preserving other projects and meta.
- `toProjectEntry` keeps tasks in document order — unlike `toStatements`, which
  topo-sorts and silently DROPS cycle-stuck tasks on .ug save (pre-existing data-loss
  quirk of the script format; TOML avoids it since load-time validation is not
  sequential).
- Tests: encode→decode identity (2 projects, meta, quoted-ish names, alias + inline
  durations) and a defaults test (omitted meta/after/description/sections).
