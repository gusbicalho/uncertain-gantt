# Web editor redesign: fluid in-place editing

Status: implemented (see ARCHITECTURE.md for the system as built, and
the amendments section at the end of this document for where the
implementation diverged). Kept for the design rationale.

## Problem

The web editor is a faithful port of the TUI's workflow-step model:
every mutation swaps the whole screen for a form, and vocabulary lives
on separate management screens. In a terminal that model is natural —
one focused pane, modal keyboard flow, no pointer. In a browser it
fights the medium:

- **Editing one field costs a full context switch.** Change a duration
  from `normal 13 2` to `normal 15 2`: click Edit → the table *and the
  estimate* vanish, replaced by a five-field form → edit one field →
  click Save → back. Four interactions and a total loss of context for
  a two-character change — and the estimate you were comparing against
  is gone from view.
- **Adding N tasks is N form round-trips**, each starting from a blank
  screen with no table visible to remind you what you already entered.
- **Vocabulary is a detour that discards work.** Realizing mid-task
  that you need a new resource means Cancel (losing the half-entered
  task), Manage, Add resource, Save, Back, re-enter the task from
  scratch.
- **The estimate is a report you request, not feedback.** After every
  change you must remember to click Run estimate (the stale note tells
  you to; it can't do it for you).
- **No undo.** A wrong edit or delete is re-entered by hand. The
  armed-confirm delete guards only referenced elements; deleting the
  wrong *unreferenced* task is instant and unrecoverable.

Meanwhile the browser has affordances the TUI lacks — a pointer that
can hit any cell directly, multiple simultaneously visible regions,
native datalist completion, and (via Hyperbole) server push — and the
current UI uses none of them beyond the datalists.

## Design principles

1. **The table is the editor.** No screen swaps for CRUD. Rows edit in
   place; vocabulary edits in place; the estimate never leaves view.
2. **One interaction per change.** Click the thing you want to change →
   type → Enter. The cost of an edit should be proportional to the size
   of the change, not to the size of the form it lives in.
3. **The estimate is live feedback.** Every committed change re-runs
   the simulation automatically; the core loop becomes *tweak → watch
   the distribution move*. Show the delta, not just the new numbers.
4. **Nothing is lost.** Half-typed rows are legal (the tolerant Doc
   already permits them — the web UI just doesn't exploit it), undo
   covers mistakes, and validation issues are actionable, not just
   diagnostic.
5. **This is a view-layer redesign.** `Editor.Doc`, `DocOp`, tolerant
   validation, `FormSpec` parsing, `Editor.Estimate` and persistence
   stay shared with the TUI, unchanged or extended compatibly.

## One screen

Everything stays on one page; nothing navigates away:

```
 my-project.toml · product build      Unsaved changes  [Undo] [Save]
 ─────────────────────────────────────────────────────────────────────
 ▸ Resources: TeamA ×2 · TeamB ×5 · TeamC ×1
 ▸ Durations: Small 1–5d · Large 21–45d · Medium ~15d ×1.6
 ─────────────────────────────────────────────────────────────────────
 TASK            RESOURCE  DURATION    AFTER          │ ESTIMATE
 Discovery       TeamA     ~13d ±2     —            × │ mean 77.5 → 81.2 (+3.7)
   Build A       TeamA     [normal 15 2______]      × │ p50 76 → 80   p95 101 → 106
   Build B       TeamB     Small       Discovery    × │
   Sell Stuff    TeamA     Large       Discovery    × │  below ▏           0.5%
     Integrate   TeamB     Large       Build A, B…  × │   50.0 ██▎         3.5%
       User Test TeamC     Medium      Integrate    × │   57.0 ██████▊     9.9%
 ┌───────────────────────────────────────────────┐    │   ...
 │ + new task…                                   │    │
 └───────────────────────────────────────────────┘    │ recalculating…
```

The `▸` vocabulary strips expand in place (see below). `Build A`'s
duration cell is shown mid-edit. The estimate shows the delta produced
by the last committed change and is already recalculating this one.

## Editing in place

**Click any cell to edit that field.** The row enters edit mode — a
form spanning the row, every field an input with its datalist — with
the clicked field focused. Enter commits the row, Escape cancels, Tab
moves between fields (browser-native). Committing applies one
`OpReplace` through the existing `editSpec`/`formParse`, so parsing,
rename-propagation and validation semantics are identical to today; the
whole-row form is also what keeps `FormSpec` reusable unchanged.

- Only *syntax* errors block a commit (an unparseable duration
  expression); the error appears under the row and the field stays
  open. Unknown resource/alias/dependency names are **not** errors —
  they commit fine and surface as the row's `!` issues, exactly the
  tolerant-builder semantics.
- Any commit re-renders that row, the vocabulary strips (usage counts),
  and kicks the live estimate.
- Descriptions: shown as today under the name; edited as one more field
  in the row form.

**The Edit button disappears** — cells are the edit affordance. Delete
shrinks to an `×` at the row end, revealed on hover (always visible on
touch devices).

## The quick-add row

The last row of the table is permanently an empty "new task" row. Type
a name, Tab through resource/duration/dependencies (with completions),
Enter commits — and focus lands in a fresh quick-add row, so entering
ten tasks is ten lines of typing, zero navigation. A name alone is
enough to commit (the missing resource/duration become `!` issues on
the row, which is precisely the "sketch first, firm up later" workflow
the tolerant model was built for).

## Vocabulary: expand in place, create from use

The two strip lines become expandable sections. Clicking `▸ Resources:`
unfolds the resource table *above the task table* (task table stays
visible below); rows edit in place with the same cell-click pattern,
plus a quick-add row. No separate screen, no Back button.

**Create-from-use:** when a task row references an undefined name, the
row's issue line becomes actionable:

```
 ! resource "TeamD" is not defined   [Define TeamD ×1]
```

One click defines it (capacity 1) and opens its capacity cell in the
expanded resources section for immediate adjustment. Same for duration
aliases (`[Define "huge" = …]` opens the definition cell). Vocabulary
gets created at the moment of need, mid-flow, instead of requiring a
premeditated detour.

## The live estimate

- **Every committed Doc change schedules an estimate run**, debounced
  (~500 ms) so a burst of quick-add commits coalesces into one run.
  Estimates already run on a forked thread; completion **pushes** the
  new report to the estimate panel via Hyperbole's `pushUpdateTo` — the
  `RunEstimate`/`PollEstimate` onLoad-polling machinery goes away.
- While recalculating, the previous report stays visible, dimmed, with
  a "recalculating…" line — numbers never blink out.
- **Show the delta.** Keep the previous report and render
  `mean 77.5 → 81.2 (+3.7)` (same for the headline quantiles), so the
  effect of the change you just made is readable without memorizing the
  old numbers. This is the payoff of the whole redesign: duration
  tweak → immediate, quantified impact on the completion distribution.
- The Run estimate button remains (to re-sample: two runs of the same
  project differ by sampling noise), plus an "auto" toggle for very
  large projects where 1000 simulations per keystroke-burst is too
  much. The stale-report note only applies when auto is off.
- Stale results are discarded by generation counter: each Doc change
  bumps an epoch; a finishing run whose epoch is outdated writes
  nothing. (Simulations are short; aborting them mid-run isn't worth
  the complexity.)

## Undo

A single undo stack of `Doc` snapshots (the Doc is a small list of
elements; snapshots are trivially correct — inverse-op bookkeeping is
an optimization we don't need). Header Undo button + Ctrl+Z. Every
`DocOp` pushes; undo pops, marks dirty, and re-runs the live estimate.

With undo in place, **replace the armed-confirm delete with an undo
toast**: any `×` deletes immediately and the status line shows
`Deleted "Build A" — Undo`. One click instead of two for the common
case, and *more* safety than today for the case the guard never
covered (deleting the wrong unreferenced element). This deliberately
diverges from the TUI, which has no undo and keeps the second-`x`
guard.

## Keyboard

Within an editing row: Enter commit · Escape cancel · Tab/Shift-Tab
between fields (all browser-native or `onKeyDown`). Quick-add: Enter
commits and refocuses. Global, later and optional: `n` focus
quick-add, Ctrl+Z undo, Ctrl+S save.

## Mobile

The card layout keeps working: tapping a field line in a card edits
that field in place (same actions, same row-form, card-shaped). The
quick-add row becomes a quick-add card; `×` is always visible; the
estimate below auto-updates the same way. Cell-tap-to-edit may prove
too twitchy on touch — if so, cards keep a small edit affordance per
line rather than making the whole line a target (open question 4).

## What this does not change

- `Editor.Doc` / `DocOp` / tolerant validation / issue derivation /
  `FormSpec` parsing / `Editor.Estimate` / TOML & `.ug` persistence.
- Explicit Save + dirty flag (autosave is an open question, not part
  of this redesign).
- The TUI keeps its current interaction model.
- Datalist completion (including its whole-value limitation for
  Depends-on).

## Implementation sketch (for later)

- **Split the single `App` HyperView** into a small set of child views
  (Hyperbole's `Require`): `Header`, `VocabSection` (×2), `TaskTable`,
  `TaskRow` (one per row), `QuickAdd`, `EstimatePanel`. A row edit
  re-renders one row, not the page; cross-view refreshes (estimate
  panel, vocab usage counts after a commit) use `trigger` /
  `pushUpdateTo`, both in Hyperbole 0.7. Row actions carry the Doc
  index they were rendered for, as today.
- **Row edit mode is a real form**, so `formParse` is reused unchanged;
  "click a cell" is just "enter edit mode with autofocus on field _i_".
  No per-field parser needed in `editor/`.
- **State stays the module-level `TVar AppState`**, gaining: undo stack
  (`[Doc]`), estimate epoch (`Int`), debounce bookkeeping, auto-toggle.
  The `Screen` type shrinks: no `SForm`, no separate panel screens —
  screen state becomes "which row is in edit mode + which vocab
  sections are expanded".
- **Phasing** (each step shippable alone):
  1. In-place row editing + quick-add row (biggest win, no new
     `editor/` logic).
  2. Live estimate: debounced auto-run, push instead of poll, delta
     line.
  3. Vocabulary inline sections + create-from-use quick fixes.
  4. Undo + delete-as-undo-toast.

## Open questions

1. **Autosave?** With undo and a dirty flag, autosave-on-commit becomes
   defensible; but silently rewriting a user's TOML file (and its
   other projects' formatting) has sharp edges. Deferred.
2. **Delete guard vs undo toast** — recommendation above is the toast;
   confirm before building step 4. *(Resolved: built the toast.)*
3. **Auto-estimate default** for large projects: always-on with the
   manual toggle, or auto-off above a task-count threshold?
   *(Shipped always-on with the toggle; revisit if it hurts.)*
4. **Touch targets:** is tap-any-line-to-edit acceptable on mobile, or
   does it need a per-card edit affordance?

## Amendments made during implementation

- **No per-row HyperViews.** A commit can re-sort the table and change
  other rows' issues, so committing re-renders the whole table anyway;
  three views (`Header`, `TaskTable`, `EstimatePanel`) turned out to be
  the natural granularity. The property that motivated row views — a
  panel update must not clobber in-progress typing — is preserved by
  the *estimate* being its own view.
- **Cross-view refreshes are `trigger`, not `pushUpdateTo`.** Form
  submissions arrive over HTTP, where Hyperbole silently drops pushes
  but delivers triggers as response metadata the client then dispatches.
- **No explicit debounce.** Hyperbole runs each action in its own
  thread and cancels the in-flight action when a newer one arrives for
  the same view, so a burst of commits simply restarts the simulation;
  the epoch counter plus the self-arming stale+auto `onLoad` element
  make the loop converge without timers.
- **No Ctrl+Z.** Hyperbole key events dispatch off the focused
  element's data attributes, so there is no global keybinding hook;
  undo is the header button (and Escape-to-cancel is an attribute on
  every form input).
