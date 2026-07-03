# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A probabilistic project planner: task durations are probability
distributions, completion times come from Monte Carlo simulation under
resource and dependency constraints. One library, two executables:

- `uncertain-gantt` — CLI that runs `.ug` scripts and a REPL.
- `uncertain-gantt-tui` — interactive project editor (reflex-vty).
  `uncertain-gantt-tui FILE [PROJECT]`; TOML files are the primary
  format, `.ug` is legacy (dispatch by extension).

## Commands

- `cabal build all` — build library, both executables, tests
- `cabal test` — run the test suite (hand-rolled assertions in `test/Spec.hs`)
- `cabal run uncertain-gantt -- resources/example.ug` — run the CLI
- `cabal run uncertain-gantt-tui -- resources/example.toml` — run the TUI
- `cabal exec -- fourmolu -i src app tui test` — format (always before committing)
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
issues plus the maximal usable project; used by the TUI). The TUI edits
a tolerant `Doc` model (`tui/Tui/Doc.hs`) that permits invalid states
and converts to a `Project` on demand via `docProjectIssues`; a single
`BuildIssue` list drives the estimate pane, the task table's `!` flags,
and the detail line.

Read `ARCHITECTURE.md` before structural changes — it includes the
layer map, dependency rules between namespaces, and a maintained list
of known refactoring opportunities (including two known CLI gantt
rendering bugs). Do not add new dependencies on `Script.*` outside the
CLI and the persistence boundary.

## Working on the TUI

- `tui/Tui/Doc.hs` (model + validation) and `tui/Tui/View.hs` (pure
  screen derivation) contain the logic; `tui/Main.hs` is FRP wiring;
  `tui/Tui/Widgets.hs` has the form/completion machinery. Design
  rationale: `tui/DESIGN.md`.
- These modules are compiled into the executable, so `cabal test`
  cannot reach them. Verify TUI changes by driving the real binary
  under a dedicated tmux server, e.g.:
  `tmux -L test new-session -d -x 120 -y 35 "TERM=xterm-256color <binary> file.toml"`,
  then `tmux -L test send-keys …` / `capture-pane -p`.
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
- `TOML-FORMAT.md` — storage format spec
- `tui/DESIGN.md` — TUI design rationale and deferred features
- `historical-docs/` — outdated point-in-time docs; do not rely on them
