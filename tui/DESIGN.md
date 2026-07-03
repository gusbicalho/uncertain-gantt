# TUI editor redesign: task-centric main screen

Status: implemented (see ARCHITECTURE.md for the system as built). Kept
for the design rationale and the deferred items below.

## Problem

The editor's main screen is a flat list whose rows are, in effect,
pretty-printed `.ug` script lines:

```
> resource Dev × 2
  duration small = uniform 1 5
  task Setup  [Dev, small]
  task Build  [Dev, normal 10.0 2.0]  after Setup
```

That leaks the storage format into the UI:

- **Three kinds of things share one list.** Resources and duration aliases
  are a small "vocabulary" you set up once and rarely touch; tasks are the
  actual content you iterate on. Mixing them means the vocabulary permanently
  occupies the top of the list and task rows start at arbitrary offsets.
- **Rows read like syntax, not data.** `task "Build A"  [TeamA, logNormal
  13.0 0.5]  after Discovery` forces the user to parse brackets, quotes and
  distribution constructors. Nobody using the TUI cares what the file looks
  like — and we may move away from `.ug` files entirely.
- **Structure is invisible.** The defining feature of a project — the
  dependency graph — is only readable by chasing `after …` clauses. There is
  no sense of which tasks are entry points, what depends on what, or how
  deep the chains go.
- **No usage context.** Deleting a resource silently breaks the tasks that
  use it (the error only shows up in the estimate pane). Nothing tells you
  "TeamA is used by 3 tasks" before you act.

## Design principles

1. **Tasks are the main screen.** Resources and durations get their own
   small management panels, entered from the task screen and left with Esc.
2. **Show data, not syntax.** Columns, not clause syntax; friendly duration
   notation, no quotes/brackets/keywords anywhere.
3. **Make the graph visible.** Topological order with indentation by
   dependency depth, so roots, fan-out and chains are readable at a glance.
4. **Show consequences before actions.** Every vocabulary row shows what
   uses it; deleting something in use asks first.
5. **Keep the interaction model.** One focused list at a time, j/k + Enter
   + x, forms (with completion) unchanged. The estimate pane and the
   split/tab view modes stay as they are.

## Main screen: the task table

```
uncertain-gantt: my-project.ug  [split view]
┌ Tasks ──────────────────────────────────────────────┐┌ Estimate ─────────────┐
│ Resources: TeamA ×2 · TeamB ×5 · TeamC ×1     [R]    ││ C-r or F5 to estimate │
│ Durations: small 1–5d · large 21–45d          [D]    ││                       │
│──────────────────────────────────────────────────────││ 1000 simulation runs  │
│   TASK             RESOURCE  DURATION        AFTER   ││                       │
│ > Discovery        TeamA     ~13d ±2         —       ││ Completion time:      │
│     Build A        TeamA     ~13d ×1.6       Discove…││   mean 77.6           │
│     Build B        TeamB     small           Discove…││   p50  76.0           │
│       Integrate    TeamB     large           Build A…││   ...                 │
│     Sell Stuff     TeamA     large           Discove…││    7.0 ####  3.7%     │
│                                                      ││    8.2 ##### 4.9%     │
│──────────────────────────────────────────────────────││   ...                 │
│ Find things out about the thing we want to build     ││                       │
└──────────────────────────────────────────────────────┘└───────────────────────┘
j/k move | Enter edit | a add | x del | R resources | D durations | C-r C-s C-q
```

Elements of the design:

- **Vocabulary strip** (top two lines): a read-only summary of resources and
  durations so the task table has context without leaving the screen. `[R]`
  and `[D]` open the management panels.
- **Task rows are columns**: name, resource, duration, dependencies. No
  quoting — names render verbatim. The AFTER column truncates with `…`
  (the full list is one Enter away in the edit form).
- **Topological order + depth indent**: tasks with no dependencies sit at
  the left margin; each row is indented by its dependency depth (longest
  path from a root, 2 spaces per level). This renders a DAG, not a tree, so
  a task with two parents simply sits one level deeper than its deepest
  parent — exact parentage stays in the AFTER column. Tasks stuck in a
  dependency cycle or referencing unknown names sort last and are marked
  `!` with the message in the estimate pane (as today).
- **Detail line** (bottom of pane): the selected task's description, dimmed.
  Descriptions leave the table; a `≡` marker after the name indicates one
  exists when not selected.
- **Selection is by task identity, not list offset.** Internally the Doc
  stays an ordered list; the view keeps a display-order→element mapping so
  edits/deletes hit the right element and the cursor follows a task when
  re-sorting moves it.

### Duration notation

One compact, unit-carrying format everywhere (table, vocabulary strip,
panels — and eventually the estimate pane):

| model                | display    | reading                          |
|----------------------|------------|----------------------------------|
| `uniform 1 5`        | `1–5d`     | between 1 and 5 days             |
| `normal 13 2`        | `~13d ±2`  | around 13 days, ±2 typical       |
| `logNormal 13 0.5`   | `~13d ×1.6`| around 13 days, ×/÷1.6 typical (e^σ) |
| alias `small`        | `small`    | expansion visible in strip/panel |

Numbers render with at most one decimal, no trailing `.0`.

## Resources panel (`R`)

Replaces the task table (same workflow mechanism as forms); Esc returns.

```
┌ Resources ──────────────────────────────────────────┐
│   NAME      CAPACITY   USED BY                      │
│ > TeamA     2          3 tasks: Discovery, Build A… │
│   TeamB     5          2 tasks: Build B, Integrate  │
│   TeamC     1          1 task:  User Test           │
│                                                     │
│                                                     │
│ a add   Enter edit   x delete   Esc back            │
└─────────────────────────────────────────────────────┘
```

- **USED BY** counts tasks referencing the resource and lists names until
  the column runs out.
- **Delete-in-use asks first.** `x` on TeamA shows, in the status bar:
  `TeamA is used by 3 tasks — x again to delete anyway`, mirroring the
  existing quit-guard pattern (any other key cancels). Deleting an unused
  resource is immediate.
- **Rename propagates.** Editing a resource's name updates every task that
  references it. Renames are by far the most common edit, and silently
  orphaning tasks would turn a rename into three follow-up fixes. (Capacity
  edits need no propagation.)

## Durations panel (`D`)

Same shape as resources:

```
┌ Durations ──────────────────────────────────────────┐
│   NAME      DEFINITION      USED BY                 │
│ > small     1–5d            2 tasks: Build B, Setup │
│   medium    ~15d ×1.6       1 task:  User Test      │
│   large     21–45d          2 tasks: Integrate, Se… │
│                                                     │
│ a add   Enter edit   x delete   Esc back            │
└─────────────────────────────────────────────────────┘
```

Same delete guard and rename propagation. The edit form is the existing
alias form (name + distribution with completion).

## Keybindings

Task screen:

| key        | action                              | change from today        |
|------------|-------------------------------------|--------------------------|
| j/k, ↓/↑   | move selection                      | unchanged                |
| Enter      | edit selected task                  | unchanged (tasks only)   |
| `a` or `t` | add task                            | was `t`                  |
| `x`        | delete selected task                | unchanged                |
| `R`        | open resources panel                | was `r` = add resource   |
| `D` or `u` | open durations panel                | was `u` = add alias      |
| C-r/F5, C-s, v, Tab, C-q | estimate, save, views, quit | unchanged      |

Panels: j/k, `a` add, Enter edit, `x` delete (guarded), Esc back.

Losing one-keystroke "add resource" from the main screen is fine: vocabulary
is set up once; `R a` is two keys.

## What this does *not* change

- The `Doc` model, forms (incl. completion), validation, estimate pane,
  split/tab views, save/load, quit guard.
- Persistence: still `.ug` scripts via `toStatements`/`fromStatements`.
  This redesign removes the *UI's* dependence on script syntax, which is
  exactly the property that lets the storage format change later without
  touching the screens.

## Implementation sketch (for later)

- `Tui.View` module: pure functions `Doc -> TaskTable` (topo sort + depth +
  column layout + width-aware truncation) and `Doc -> [ResourceRow]` /
  `[DurationRow]` (usage joins). All unit-testable without reflex.
- `Main.hs`: workflow gains two panel steps alongside the form steps;
  selection state becomes per-screen and identity-based (task/resource/alias
  name) instead of a single Doc index.
- Rename propagation: a new `DocOp` (`OpRenameResource`, `OpRenameAlias`)
  applied atomically so undo/redo (future) stays element-grained.
- Column widths: fixed minimums with the name column flexing; reuse the
  `Text.take`-based truncation from `selectList`.

## Resolved questions

1. **Per-task simulation stats** (`START`/`END` columns from the last run):
   deferred.
2. **In-place description editing:** deferred; descriptions stay in the
   task form.
3. **Vocabulary strip overflow:** two lines is the budget. Each line
   truncates with `+n more`; the full information is always available in
   the corresponding panel (`R`/`D`), and each panel shows the selected
   row's complete, untruncated info (full used-by list) in a detail line
   at the bottom of the panel.

## Amendments made during implementation

- **Validation and the estimate pane evolved after this design.** The
  "unchanged" items below reflect the state at design time; since then,
  validation moved to a tolerant builder (all issues reported at once,
  estimates run on the usable subset) and the `!` flags/detail line are
  driven by those issues. See ARCHITECTURE.md.

- **Task deletes are guarded too.** Deleting a task that other tasks list
  as a dependency uses the same second-`x` confirmation as vocabulary
  deletes ("Build A is a dependency of 2 tasks — x again to delete").
- **Task renames propagate** to dependents' dependency lists, same
  rationale as resource/alias renames.
