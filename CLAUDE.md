# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A probabilistic project planner: task durations are probability
distributions, completion times come from Monte Carlo simulation under
resource and dependency constraints. One library, three executables:

- `uncertain-gantt` — CLI that runs `.ug` scripts and a REPL.
- `uncertain-gantt-tui` — interactive project editor (reflex-vty).
  `uncertain-gantt-tui FILE [PROJECT]`; TOML files are the primary
  format, `.ug` is legacy (dispatch by extension).
- `uncertain-gantt-web` — the same editor in the browser (Hyperbole),
  serving on `http://localhost:3000` by default (`--port` to change).
  `uncertain-gantt-web PATH [PROJECT]`, where `PATH` is a directory to
  browse or a single file to open; the open document is addressed by
  URL and any number can be open at once.

## Commands

- `cabal build all` — build library, all executables, tests
- `cabal test` — run the test suite (hand-rolled assertions in `test/Spec.hs`)
- `cabal run uncertain-gantt -- resources/example.ug` — run the CLI
- `cabal run uncertain-gantt-tui -- resources/example.toml` — run the TUI
- `cabal run uncertain-gantt-web -- resources/` — run the web editor (or
  pass a single file: `-- resources/example.toml`)
- `cabal exec -- fourmolu -i src app tui web editor test` — format (always before committing)
- `./release.sh` — currently broken (hardcodes an old GHC path)

Toolchain: GHC 9.12.4, `default-language: GHC2021`, warnings are
extensive (`-Wall` plus more) and the build is expected warning-free.
`monad-bayes` is pinned to a git commit in `cabal.project`.

The vendored `vendor/tricorder` submodule provides a build-diagnostics
daemon (`tricorder start`, `tricorder status --wait`) backed by a
persistent GHCi session — faster feedback than `cabal build` for
incremental changes. Warm `dist-newstyle` with a plain `cabal build`
first if heavy dependencies changed.

## Architecture in one paragraph

Domain core (`Task`, `Project`, `Simulator`, `Gantt`) is polymorphic in
resource/duration types and imports nothing from the outer layers.
`Lang.*` is the definition vocabulary + expression syntax shared by all
frontends; `Sim.*` is sampling/Monte Carlo/statistics; `Toml` and
`Script.*` are the two storage formats (TOML primary, `.ug` legacy —
the script interpreter stack is CLI-only). Two project builders exist:
strict `BuildProjectM` (fail-fast, order-sensitive; used by the script
interpreter) and `Project.Tolerant` (collect-then-validate, returns all
issues plus the maximal usable project; used by the editors). The TUI
and web editors share the `editor/` layer: a tolerant `Doc` model
(`editor/Editor/Doc.hs`) that permits invalid states and converts to a
`Project` on demand via `docProjectIssues`; a single `BuildIssue` list
drives the estimate pane, the task table's `!` flags, and the detail
line. `editor/` is UI-framework-free — reflex-vty code lives only in
`tui/`, Hyperbole code only in `web/`.

Read `ARCHITECTURE.md` before structural changes — it includes the
layer map, dependency rules between namespaces, and a maintained list
of known refactoring opportunities (including two known CLI gantt
rendering bugs). Do not add new dependencies on `Script.*` outside the
CLI and the persistence boundary.

## Working on the editors (TUI and web)

- `editor/Editor/Doc.hs` (model + validation) and `editor/Editor/View.hs`
  (pure screen derivation) contain the shared logic; `tui/Main.hs` is FRP
  wiring; `tui/Tui/Widgets.hs` has the form/completion machinery. The
  Hyperbole app is split across `web/Web/`: `Route.hs` (URL shapes,
  `DocKey`), `Docs.hs` (plain `DocState`/`ServerState` data), `Capability.hs`
  (`DocsSurface`/`DocHandle` interfaces, dot-only via `NoFieldSelectors`),
  `State.hs` (the concrete `TVar`-backed adapter implementing them, plus
  `Adapters`), `Core.hs` (pure web-side policy — no capabilities, no
  effect row), `Editor.hs` (one document's three views), `Files.hs`
  (browser + open-files strip), `Styles.hs`, `App.hs` (routing). Design
  rationale: `tui/DESIGN.md`, `web/DESIGN.md`.
- **`web/` follows `STRUCTURING-HASKELL.md`** — read it before adding
  state or a new way to reach the outside world there. The rules that
  bite most often: no `IOE` in a signature that isn't an adapter or
  `Main`; a Surface hands out capabilities and never performs an effect
  taking a `DocKey`; a capability closes over its object at construction
  instead of taking it per call; decisions that are neither rendering
  nor storage go in `Web.Core`, not inline in an `update`. Two
  deliberate deviations: field names are prefixed (`dhModify`,
  `docsOpen`) with `NoFieldSelectors` rather than imported qualified as
  §9 suggests, which makes record-dot the only way to invoke a
  capability method; and `Web.Editor` needs Hyperbole's dynamic `Reader`
  alongside our static one (imported as `Hyp`), so §6's "prefer Static"
  applies only to `Adapters`. The TUI and CLI do not follow the
  handbook.
- These modules are compiled into the executables, so `cabal test`
  cannot reach them. Verify TUI changes by driving the real binary
  under a dedicated tmux server, e.g.:
  `tmux -L test new-session -d -x 120 -y 35 "TERM=xterm-256color <binary> file.toml"`,
  then `tmux -L test send-keys …` / `capture-pane -p`.
  Verify web changes by running the binary against a scratch *directory*
  of TOML files and driving it with a browser (Playwright MCP works
  well). Actions can also be driven with `curl`: POST to the page URL
  with `Hyp-ViewId`, `Hyp-Action`, `Hyp-RequestId` and `Hyp-State: []`
  headers (`()` is not valid JSON and makes the request parse as a plain
  page load), form fields as the body.
- reflex-vty gotchas that are easy to reintroduce: request initial
  focus by `FocusId` (`tile'` + `Refocus_Id`) — `Refocus_Shift` at
  post-build silently does nothing; key events only reach widgets
  whose `tile` has focus.
- Keep names user-facing via `ToText`, never `show` (Symbols `show` as
  `Symbolize.intern "…"`).

## File formats

- TOML: spec in `TOML-FORMAT.md` (multi-project files, `meta` tables,
  evolution rules — writers must model every key they intend to keep).
  Codecs in `src/UncertainGantt/Toml.hs`; round-trip tests in
  `test/Spec.hs`. Example: `resources/example.toml`.
- `.ug` scripts: grammar lives in `Script.Parser` + `Lang.Parser`;
  `resources/example.ug` is the authoritative syntax reference.

## More information

- `ARCHITECTURE.md` — current system description + refactor list
- `STRUCTURING-HASKELL.md` — the architecture handbook `web/` follows
- `TOML-FORMAT.md` — storage format spec
- `tui/DESIGN.md` — TUI design rationale and deferred features
- `web/DESIGN.md` — the web UI's fluid-editing design rationale
- `historical-docs/` — outdated point-in-time docs; do not rely on them
