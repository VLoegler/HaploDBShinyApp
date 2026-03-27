# HaploDB v2

A Shiny web application for the HaploTeam yeast research lab. Provides a web interface to browse, add, and visualize strain data stored in a SQLite database.

## Features

- **Browse Database** — search, filter, and download strain/sample data
- **Add Entries** — submit new YJS samples and strains (with auto-naming)
- **Review Entries** — admin approval workflow for pending submissions
- **Phylogenetic Trees** — interactive tree visualization (3034 S. cerevisiae, 1060 B. bruxellensis)
- **User Management** — admin panel for account management

## Prerequisites

- **R** >= 4.3
- **System libraries**: `libsodium-dev`, `libcurl4-openssl-dev`, `libssl-dev`, `libxml2-dev`
- **Docker** (optional, for local testing)

## Quick Start

### 1. Configure

```bash
cp config.yml.example config.yml
# Edit config.yml if needed (default paths work out of the box)
```

### 2. Install Dependencies

```r
install.packages("renv")
renv::restore()
```

### 3. Set Up Data

The app ships with a demo SQLite database in `data/haploDB/haplodb.sqlite` containing sample data (60 strains, 200 YJS samples). To use your real data, replace this file with your production SQLite database.

The `data/pending.sqlite` and `data/users.sqlite` files are created automatically on first launch.

### 4. Run

```r
shiny::runApp(".")
```

The app opens with a login screen. Default admin credentials: `admin` / `changeme` (configured in `config.yml`).

## Docker

Build and run locally:

```bash
docker compose up --build
```

The app will be available at `http://localhost:3838`.

The `data/` directory is mounted as a volume, so SQLite files persist across container restarts. Place your `config.yml` in the app root before running.

## Project Structure

```
HaploDBShinyApp/
├── app.R                 # Entry point
├── config.yml.example    # Configuration template
├── R/                    # Modules and utilities (auto-loaded)
│   ├── mod_login.R       # Authentication
│   ├── mod_browse.R      # Browse database
│   ├── mod_add_entry.R   # Add entries (YJS + Strains)
│   ├── mod_review.R      # Review pending (admin)
│   ├── mod_tree.R        # Phylogenetic trees
│   ├── mod_admin.R       # User management (admin)
│   ├── mod_home.R        # Home dashboard
│   ├── mod_notifications.R # Submission notifications
│   ├── mod_nameconv.R    # Name conversion search
│   ├── utils_db.R        # Database helpers
│   └── utils_auth.R      # Authentication helpers
├── data/
│   ├── haploDB/          # Main database directory
│   │   └── haplodb.sqlite  # Main DB (demo data, replace with real)
│   ├── pending.sqlite    # Pending submissions (auto-created)
│   ├── users.sqlite      # User accounts (auto-created)
│   └── trees/            # Phylogenetic tree data (.nwk, .csv)
├── www/                  # Static assets (logo, CSS)
├── tests/                # Unit tests (testthat)
├── docs/                 # Architecture docs
├── Dockerfile            # Container image
├── docker-compose.yml    # Local dev with volume mounts
├── .dockerignore
├── .gitignore
└── renv.lock             # Dependency lockfile
```

## Configuration

All configuration is in `config.yml` (gitignored). See `config.yml.example`:

| Key | Description |
|-----|-------------|
| `database.main_path` | Path to the main SQLite database |
| `database.pending_path` | Path to pending submissions SQLite |
| `database.users_path` | Path to users SQLite |
| `admin.username` | Default admin username |
| `admin.password` | Default admin password (hashed on first launch) |

## Database Architecture

The app uses three separate SQLite files:

- **`haplodb.sqlite`** — Main database with all strain/sample data (12 tables). WAL mode enabled for concurrent reads.
- **`pending.sqlite`** — Pending submissions awaiting admin review, custom field options, and notifications.
- **`users.sqlite`** — User accounts and credentials. Separate from main DB so you can swap the data DB without losing accounts.

All connections use WAL journal mode and a 5-second busy timeout for safe concurrent access.

The full database schema (all tables, columns, and foreign keys) is documented in [docs/schema.sql](docs/schema.sql).

## Production Deployment

### Files to prepare

Before deploying, you need to set up the following files:

| File | Action | Notes |
|------|--------|-------|
| `config.yml` | Create from `config.yml.example` | Set a strong admin password. Adjust database paths if needed. |
| `data/haploDB/haplodb.sqlite` | Replace with production data | The shipped file contains demo data (60 strains, 200 YJS). Your real database must follow the same schema (see [docs/schema.sql](docs/schema.sql)). |
| `data/users.sqlite` | Optional — auto-created on first launch | If migrating from an existing deployment, copy it over to preserve user accounts. |
| `data/pending.sqlite` | Optional — auto-created on first launch | If migrating, copy it to preserve pending submissions and notifications. |
| `data/trees/*.nwk, *.csv` | Replace or keep | Phylogenetic tree data. Replace with your own Newick/CSV files if needed. |

`config.yml`, all SQLite files, and WAL/SHM sidecar files are gitignored — they must be provided outside of version control.

### With Docker (recommended)

1. **Prepare configuration and data**

```bash
cp config.yml.example config.yml
# Edit config.yml: set a strong admin password, adjust paths if needed
# Replace data/haploDB/haplodb.sqlite with your production database
```

2. **Build and start**

```bash
docker compose up -d --build
```

The app runs on port 3838. The `data/` directory and `config.yml` are mounted as volumes, so data persists across container restarts and image rebuilds.

3. **Put behind a reverse proxy**

Do not expose port 3838 directly. Use Nginx or Caddy as a reverse proxy with HTTPS.

Nginx example (`/etc/nginx/sites-available/haplodb`):

```nginx
server {
    listen 443 ssl;
    server_name haplodb.example.com;

    ssl_certificate     /etc/letsencrypt/live/haplodb.example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/haplodb.example.com/privkey.pem;

    location / {
        proxy_pass http://127.0.0.1:3838;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_read_timeout 86400;
    }
}
```

The `Upgrade` and `Connection` headers are required for Shiny's WebSocket connections.

4. **Backups**

Back up the three SQLite files regularly. Since WAL mode is enabled, use `.backup` to get a consistent snapshot:

```bash
sqlite3 data/haploDB/haplodb.sqlite ".backup /backups/haplodb-$(date +%F).sqlite"
sqlite3 data/users.sqlite ".backup /backups/users-$(date +%F).sqlite"
sqlite3 data/pending.sqlite ".backup /backups/pending-$(date +%F).sqlite"
```

### With Shiny Server

1. Install [Shiny Server](https://posit.co/products/open-source/shinyserver/) and the app's R dependencies (`renv::restore()`)
2. Copy the app directory to `/srv/shiny-server/haplodb/`
3. Place your `config.yml` and data files in the app directory
4. Shiny Server serves the app automatically (default port 3838)

### Security checklist

- [ ] Change the default admin password in `config.yml`
- [ ] Serve over HTTPS (reverse proxy with TLS)
- [ ] Restrict network access to port 3838 (only the reverse proxy should reach it)
- [ ] Set up regular SQLite backups
- [ ] Keep R packages and the base image up to date

## Running Tests

```r
testthat::test_dir("tests/testthat")
```

## License

Internal use only — HaploTeam, Unistra.
