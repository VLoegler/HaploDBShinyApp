# HaploDB v2 — Architecture

## High-Level Overview

```
Browser
  │
  ▼
Shiny Server (10.20.5.101:3838)
  │
  ├── app.R (entry point)
  │     ├── bs_theme() — Bootstrap 5 with brand colors
  │     ├── page_navbar — 5 tabs
  │     ├── MySQL pool (production data)
  │     └── SQLite conn (pending submissions)
  │
  ├── R/ (auto-loaded modules)
  │     ├── mod_login.R    → login modal
  │     ├── mod_browse.R   → DT table + download
  │     ├── mod_add_entry.R → forms + validation + SQLite insert
  │     ├── mod_review.R   → approve/reject + MySQL insert
  │     ├── mod_tree.R     → ggtree rendering
  │     └── mod_admin.R    → user CRUD
  │
  ├── MySQL (10.20.5.100:3306, haploDB)
  │     └── 12 tables + users table
  │
  └── SQLite (data/pending.sqlite)
        └── 7 pending tables
```

## Module Map

### `R/mod_login.R`
- **UI**: `login_modal_ui(id)` — modal dialog with logo, username/password fields
- **Server**: `login_server(id, pool)` — validates credentials, returns reactive with `logged_in` and `info`

### `R/mod_browse.R`
- **UI**: `browse_ui(id)` — card with DT table, refresh button, TSV download
- **Server**: `browse_server(id, pool, user_info)` — queries `YJSnumbers LEFT JOIN Strains`, renders DT with column visibility and per-column filters

### `R/mod_add_entry.R`
- **UI**: `add_entry_ui(id)` — navset with 2 sub-tabs (YJS Samples, Strains)
- **Server**: `add_entry_server(id, pool, sqlite_conn, user_info, species_list)`
- **Pure functions** (testable):
  - `validate_yjs(df, pool, sqlite, species)` — all-or-nothing validation
  - `validate_strains(df, pool, sqlite, species)` — strain validation
  - `generate_yjs_numbers(start, count)` — YJS auto-numbering
  - `next_xtra_names(pool, sqlite, count)` — XTRA_XXX auto-naming
  - `na_if_empty(x)` — helper

### `R/mod_review.R`
- **UI**: `review_ui(id)` — table selector, DT with selection, approve/reject buttons
- **Server**: `review_server(id, pool, sqlite_conn, user_info)`
- **Helpers**:
  - `approve_yjs_row(row, pool, sqlite)` — INSERT into MySQL + DELETE from SQLite
  - `approve_strain_row(row, pool, sqlite)` — INSERT into Strains + AltNames + DELETE

### `R/mod_tree.R`
- **UI**: `tree_ui(id)` — sidebar with controls, main area with plot
- **Server**: `tree_server(id, user_info)`
- **Data loading**: `load_tree_datasets()` — reads newick + CSVs at startup
- **Rendering**: `render_tree(dataset, layout, ...)` — generic ggtree rendering

### `R/mod_admin.R`
- **UI**: `admin_ui(id)` — user table + add/delete forms
- **Server**: `admin_server(id, pool, user_info)` — CRUD operations on users table

### `R/utils_db.R`
- `create_mysql_pool(config)` — pool::dbPool for MySQL
- `create_sqlite_conn(path)` — DBI::dbConnect for SQLite
- `ensure_pending_tables(sqlite_conn)` — creates all 7 pending tables
- `ensure_users_table(pool)` — creates users table in MySQL
- `seed_default_admin(pool, admin_config)` — creates default admin if none exists

### `R/utils_auth.R`
- `check_credentials(username, password, pool)` — verifies against users table
- `is_admin(user_info)` — role check
- `hash_password(password)` — sodium wrapper

## Data Flow

### Adding entries (user -> SQLite -> MySQL)

```
1. User fills form (or uploads file) in Tab 2
2. Preview: validate_yjs() or validate_strains() checks all rows
3. Submit: rows inserted into SQLite pending table with submitted_by metadata
4. Admin opens Tab 3, selects rows, clicks Approve
5. approve_*_row() inserts into MySQL, deletes from SQLite
   - For strains: also creates AltNames entry in MySQL
```

### Authentication Flow

```
1. App starts → login modal shown
2. User enters credentials → check_credentials() queries MySQL users table
3. On success → modal dismissed, tabs shown based on role
4. Logout → session reloads (no session persistence)
```

### Approval Workflow

```
User (admin or basic) submits entry
          │
          ▼
  SQLite pending_* table
          │
    Admin reviews (Tab 3)
     ┌────┴────┐
     │         │
  Approve    Reject
     │         │
     ▼         ▼
  MySQL     DELETE from
  INSERT    SQLite
  + DELETE
  from SQLite
```

**Constraint**: Admin cannot approve their own submissions (server-side enforced).

## Database Schema

### SQLite (12 tables + users)

**Central**: `YJSnumbers` (PK: YJS_NUMBER, 26 columns, FK → Strains; CHECK: BOX_NUMBER or PLATE+PLATE_ROW+PLATE_COL)

**Strain info**:
- `Strains` (PK: STRAIN)
- `AltNames` (auto-increment ID, FK → Strains)
- `AltYJS` (FK → YJSnumbers, external)

**Sequencing**: `SeqData`, `Genotypes`, `Assemblies` (FK → YJSnumbers, Projects)

**Phenotyping**: `GrowthPhenotypes`, `RNASeqPhenotypes`, `ProteomicsPhenotypes` (FK → YJSnumbers, Conditions, Projects)

**Other**: `Conditions` (PK: ID_CONDITION), `Projects` (PK: ID_Project)

**Auth**: `users` (id, username, password_hash, role, created_at)

### SQLite Pending Tables

| Table | Mirrors | Extra Columns |
|-------|---------|---------------|
| `pending_yjs` | `YJSnumbers` | id, submitted_by, submitted_at |
| `pending_strains` | `Strains` | id, original_name, submitted_by, submitted_at |
| `pending_altnames` | `AltNames` | id, submitted_by, submitted_at |
| `pending_seqdata` | `SeqData` | id, submitted_by, submitted_at |
| `pending_growth` | `GrowthPhenotypes` | id, submitted_by, submitted_at |
| `pending_rnaseq` | `RNASeqPhenotypes` | id, submitted_by, submitted_at |
| `pending_proteomics` | `ProteomicsPhenotypes` | id, submitted_by, submitted_at |

v1 actively uses: `pending_yjs`, `pending_strains`. Others are created but unused.

## Configuration Reference

All keys in `config.yml`:

```yaml
default:
  mysql:
    host: "10.20.5.100"       # MySQL server hostname
    port: 3306                 # MySQL port
    dbname: "haploDB"          # Database name
    user: "haploadmin"         # Read-write user
    password: "..."            # Password (never commit)
  mysql_readonly:
    user: "basic_user"         # Read-only user (for direct queries)
    password: "..."
  admin:
    username: "admin"          # Default admin username
    password: "changeme"       # Default admin password (hashed on first launch)
  species:                     # Allowed species for validation
    - "Saccharomyces cerevisiae"
    - "Saccharomyces paradoxus"
    - "Brettanomyces bruxellensis"
    - "Candida albicans"
```

## Tree Data

Files in `data/trees/`:

| File | Description | Source |
|------|-------------|--------|
| `sace_3034.nwk` | Newick tree, 3034 S. cerevisiae | `PlotPhylogeny/3034 Sace/Fig2/InputFiles/` |
| `sace_3034_clades.csv` | Clade/SuperClade annotations (3048 rows) | Same, filtered from operational table |
| `sace_3034_colors.csv` | 43 clade/super-clade colors | Same |
| `brbr_1060.nwk` | Newick tree, 1060 B. bruxellensis | `PlotPhylogeny/1060 Brbr/TreeBrett/Data/` |
| `brbr_1060_clusters.csv` | Cluster annotations (1060 rows) | Same, filtered from TableS1 |

B. bruxellensis cluster colors are hardcoded in `mod_tree.R` (7 clusters).
