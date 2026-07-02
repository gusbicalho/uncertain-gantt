# TOML project file format

Status: implemented. Parsed and written with
[`tomland`](https://hackage.haskell.org/package/tomland) via
`UncertainGantt.Toml`; the TUI reads and writes `.toml` files natively
(`.ug` scripts remain supported for loading/saving legacy projects).

## Goals

1. **Multiple projects per file.** A file is a list of projects; tools
   operate on one project at a time and must preserve the others when
   saving.
2. **Metadata can be added later.** Every project carries an optional
   free-form `meta` table of string values that tools preserve even when
   they don't understand the keys. Unknown keys elsewhere are ignored on
   read, so future typed fields can be introduced without breaking old
   files (see *Evolution rules* below).
3. **Human-editable.** Plain TOML, no clever encodings; names are ordinary
   TOML strings with no extra quoting layer.

## Example

```toml
[[project]]
name = "backend-rewrite"

[project.meta]              # optional; any string-valued keys
owner = "gus"
status = "draft"

[[project.resource]]
name = "TeamA"
capacity = 2

[[project.resource]]
name = "QA people"
capacity = 1

[[project.duration]]
name = "small"
distribution = "uniform 1 5"

[[project.task]]
name = "Discovery"
resource = "TeamA"
duration = "normal 13 2"
description = "Find things out"

[[project.task]]
name = "Build"
resource = "TeamA"
duration = "small"
after = ["Discovery"]

[[project]]                 # a second project in the same file
name = "conference-talk"

[[project.resource]]
name = "Me"
capacity = 1

[[project.task]]
name = "Slides"
resource = "Me"
duration = "logNormal 3 0.5"
```

## Reference

### Top level

| key | type | meaning |
|---|---|---|
| `[[project]]` | array of tables | the projects in this file, in order |

### `[[project]]`

| key | type | required | meaning |
|---|---|---|---|
| `name` | string | yes | project name; tools address projects in a file by name |
| `meta` | table of strings | no (default empty) | free-form metadata, preserved round-trip |
| `resource` | array of tables | no (default empty) | resource definitions |
| `duration` | array of tables | no (default empty) | duration alias definitions |
| `task` | array of tables | no (default empty) | task definitions |

### `[[project.resource]]`

| key | type | required | meaning |
|---|---|---|---|
| `name` | string | yes | resource name |
| `capacity` | integer ≥ 0 | yes | how many tasks can use it concurrently |

### `[[project.duration]]`

| key | type | required | meaning |
|---|---|---|---|
| `name` | string | yes | alias name |
| `distribution` | string | yes | a distribution expression (see below); aliases cannot reference other aliases |

### `[[project.task]]`

| key | type | required | meaning |
|---|---|---|---|
| `name` | string | yes | task name |
| `resource` | string | yes | name of the resource the task occupies |
| `duration` | string | yes | a distribution expression or the name of a duration alias |
| `after` | array of strings | no (default `[]`) | names of tasks this one depends on |
| `description` | string | no (default `""`) | free-text description |

### Distribution expressions

The `distribution` and `duration` values use the expression syntax of the
definition language (`UncertainGantt.Lang.Parser`), the same syntax typed
into the TUI's form fields:

- `uniform A B` — uniformly between `A` and `B` days (integers)
- `normal AVG DEV` — normal distribution (decimals allowed)
- `logNormal MEDIAN DEV` — log-normal with median and log-scale deviation

In a task's `duration`, anything that doesn't parse as a distribution is
an alias reference. An alias whose name is not purely alphanumeric is
written in embedded quotes, exactly as in form fields: `duration =
'"my odd alias"'`. All *other* name fields (`name`, `resource`, `after`)
are plain strings with no embedded quoting.

*Why strings rather than structured tables* (e.g. `duration = { type =
"normal", avg = 13, dev = 2 }`): the expression syntax is already the
shared surface language of the project — the TUI form fields and the
`.ug` format both speak it — and it round-trips through
`Lang.Parser`/`Lang.Render`, which are tested against each other. One
syntax everywhere beats two.

## Evolution rules

For future format changes, the compatibility contract is:

1. **Readers ignore unknown keys.** Adding a new optional key (to a
   project, task, resource, or duration table) is always
   backward-compatible: old tools read the file, but note rule 3.
2. **`meta` is the forward-compatible slot.** String-valued metadata needs
   no format change at all, and every tool preserves the whole `meta`
   table verbatim. Once a piece of metadata needs structure or typing, it
   graduates to a real key (rule 1).
3. **Writers preserve what they model — nothing else.** A tool that
   rewrites a file re-encodes it from its model, so unknown keys are
   dropped on *write* even though they're accepted on read. Until
   position-preserving editing exists, new typed keys should therefore be
   added to the codec (even if unused) in the same release that anything
   starts writing them.
4. **Never change the meaning or type of an existing key.** Pick a new
   name instead.

## Editor behavior (uncertain-gantt-tui)

- `uncertain-gantt-tui file.toml` opens the first project in the file;
  `uncertain-gantt-tui file.toml NAME` opens the project named `NAME`
  (the error for an unknown name lists what's available). A missing file
  starts a new single-project file (project named after the file, or
  `NAME` if given).
- Saving re-encodes the whole file, replacing only the edited project and
  keeping all other projects as loaded. The project's `meta` table is
  carried through untouched.
- Files ending in `.ug` load and save in the legacy script format;
  everything else is TOML.
