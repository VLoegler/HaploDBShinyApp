# Contributing to HaploDB v2

## How to Add a New Tab

1. Create `R/mod_<name>.R` with two functions:
   - `<name>_ui(id)` — returns the UI (using `NS(id)` for namespacing)
   - `<name>_server(id, ...)` — uses `moduleServer(id, function(input, output, session) { ... })`

2. In `app.R`:
   - Add `nav_panel("Tab Name", value = "tab_value", <name>_ui("ns_id"))` in the UI
   - Add `<name>_server("ns_id", ...)` in the server function
   - If admin-only: add `toggle_tab("tab_value", admin)` in the observe block

3. Files in `R/` are auto-loaded by Shiny. No `source()` needed.

## How to Add a Sub-Tab to Add Entry (Tab 2)

1. In `R/mod_add_entry.R`:
   - Add a new `nav_panel(...)` inside the `navset_card_tab()` in `add_entry_ui()`
   - Add validation logic as a pure function (e.g., `validate_seqdata()`)
   - Add preview/submit event handlers in `add_entry_server()`

2. Create the corresponding SQLite pending table in `R/utils_db.R` → `ensure_pending_tables()` (it may already exist for deferred tables).

3. In `R/mod_review.R`:
   - Add the new pending table to the `selectInput` choices
   - Add an `approve_<type>_row()` helper function

## How to Add a Validation Rule

Validation functions are in `R/mod_add_entry.R` and are pure functions (no Shiny dependency). They take a data frame and return `list(valid = TRUE/FALSE, errors = character())`.

To add a rule:
1. Add the check inside the `for (i in seq_len(nrow(df)))` loop in the relevant `validate_*()` function
2. Append to the `errors` vector with a descriptive message prefixed with `sprintf("Row %d: ", i)`
3. Add a test in `tests/testthat/test-validation.R`

## Code Style

- **Snake_case** for all function and variable names
- **Module function prefix**: `browse_ui`, `browse_server`, `add_entry_ui`, etc.
- **No `eval(parse())`** — build queries and plots programmatically
- **Parameterized queries** (`DBI::dbExecute(conn, "... WHERE x = ?", params = list(val))`) — never paste user input into SQL
- **Comments**: only for non-obvious "why", not for "what"
- Keep functions under ~50 lines; extract helpers when they grow

## Running Tests

```r
# From the project root
testthat::test_dir("tests/testthat")
```

Tests use in-memory SQLite databases to mock MySQL. No real database connection needed.

## Dependency Management

```r
# After adding a new package
renv::snapshot()

# To restore on a new machine
renv::restore()
```

## File Conventions

| Pattern | Purpose |
|---------|---------|
| `R/mod_*.R` | Shiny module (UI + server) |
| `R/utils_*.R` | Utility functions (no Shiny dependency) |
| `data/trees/` | Static tree data (newick, CSVs) |
| `www/` | Static web assets (images, CSS) |
| `tests/testthat/test-*.R` | Unit tests |
