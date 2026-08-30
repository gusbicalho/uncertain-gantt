# Historical documents

Point-in-time documents kept for reference. **Do not use these to learn
how the system works today** — the current documentation is
`ARCHITECTURE.md` at the repository root.

- `ARCHITECTURE-2021.md`, `WALKTHROUGH-2021.md` — early descriptions of
  the project. Both show DSL syntax the parser never accepted; superseded
  by `ARCHITECTURE.md` and `resources/example.ug`.
- `ARCHITECTURE-REVIEW-2026-07.md` — a full-codebase review as of commit
  `aeed053`. Module paths predate the Lang/Sim namespace extraction, and
  several findings (triple validation, all-or-nothing estimates) have
  since been fixed. Its unfixed findings are carried forward in the
  "Refactoring opportunities" section of the current `ARCHITECTURE.md`.
- `session-notes-2026-07.md` — working notes from the development
  sessions that built the TUI editor, the TOML format and the tolerant
  builder. Useful for the rationale behind specific decisions.
