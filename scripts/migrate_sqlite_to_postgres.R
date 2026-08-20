#!/usr/bin/env Rscript

# ============================================================================
# HaploDB SQLite -> PostgreSQL Migration
# ============================================================================
#
# Migration sources:
#   - data/haploDB/haplodb.sqlite
#   - data/users.sqlite
#   - data/pending.sqlite
#
# Target:
#   - PostgreSQL (Neon)
#
# ============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(RPostgres)
  library(dotenv)
})

# ----------------------------------------------------------------------------
# Load environment variables
# ----------------------------------------------------------------------------

dotenv::load_dot_env()

required_vars <- c(
  "DB_HOST",
  "DB_NAME",
  "DB_USER",
  "DB_PASSWORD",
  "DB_PORT"
)

missing_vars <- required_vars[
  Sys.getenv(required_vars) == ""
]

if (length(missing_vars) > 0) {
  stop(
    sprintf(
      "Missing environment variables: %s",
      paste(missing_vars, collapse = ", ")
    )
  )
}

message("✓ Environment variables loaded")

# ----------------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------------

HAPLODB_PATH <- "data/haploDB/haplodb.sqlite"
USERS_PATH <- "data/users.sqlite"
PENDING_PATH <- "data/pending.sqlite"

# ----------------------------------------------------------------------------
# Database connections
# ----------------------------------------------------------------------------

message("Connecting to PostgreSQL...")

pg_con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  dbname = Sys.getenv("DB_NAME"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  port = as.integer(Sys.getenv("DB_PORT")),
  sslmode = "require"
)

message("✓ Connected to PostgreSQL")

message("Opening SQLite databases...")

main_sqlite <- dbConnect(
  RSQLite::SQLite(),
  HAPLODB_PATH
)

users_sqlite <- dbConnect(
  RSQLite::SQLite(),
  USERS_PATH
)

pending_sqlite <- dbConnect(
  RSQLite::SQLite(),
  PENDING_PATH
)

message("✓ Connected to haplodb.sqlite")
message("✓ Connected to users.sqlite")
message("✓ Connected to pending.sqlite")

# ----------------------------------------------------------------------------
# Verify PostgreSQL connection
# ----------------------------------------------------------------------------

db_version <- dbGetQuery(
  pg_con,
  "SELECT version();"
)

message("PostgreSQL version:")
message(db_version$version[1])

message("Loading PostgreSQL schema...")

# ----------------------------------------------------------------------------
# Build Schema on PostgreSQL DB
# ----------------------------------------------------------------------------

sql_text <- paste(
  readLines("sql/postgresql_schema.sql", warn = FALSE),
  collapse = "\n"
)

statements <- strsplit(sql_text, ";", fixed = TRUE)[[1]]

for (stmt in statements) {
  stmt <- trimws(stmt)

  if (nzchar(stmt)) {
    DBI::dbExecute(pg_con, stmt)
  }
}

message("✓ Schema loaded")

# ----------------------------------------------------------------------------
# Define migration table mappings
# ----------------------------------------------------------------------------

main_tables <- c(
  Strains = "strains",
  YJSnumbers = "yjs_numbers",
  AltNames = "alt_names",
  AltYJS = "alt_yjs",
  Projects = "projects",
  Conditions = "conditions",
  SeqData = "seq_data",
  Genotypes = "genotypes",
  Assemblies = "assemblies",
  GrowthPhenotypes = "growth_phenotypes"
)

users_tables <- c(
  users = "users"
)

pending_tables <- c(
  pending_yjs = "pending_yjs",
  pending_strains = "pending_strains",
  pending_altnames = "pending_altnames",
  pending_seqdata = "pending_seqdata",
  pending_growth = "pending_growth",
  custom_options = "custom_options",
  notifications = "notifications"
)

message("✓ Migration mappings loaded")

# ----------------------------------------------------------------------------
# Utility functions
# ----------------------------------------------------------------------------

copy_table <- function(sqlite_con,
                       sqlite_table,
                       pg_con,
                       pg_table) {

  message(sprintf("Migrating %s -> %s ...",
                  sqlite_table,
                  pg_table))

  df <- DBI::dbReadTable(sqlite_con, sqlite_table)

  # --------------------------------------------------------------------------
  # Column name mappings
  # --------------------------------------------------------------------------

  if ("USER" %in% names(df)) {
    names(df)[names(df) == "USER"] <- "user_name"
  }

  names(df) <- tolower(names(df))
  if (pg_table == "seq_data") {
    names(df)[names(df) == "id_seqdata"] <- "id_seq_data"
  }

  # --------------------------------------------------------------------------
  # Import
  # --------------------------------------------------------------------------

  if (nrow(df) > 0) {

    DBI::dbWriteTable(
      pg_con,
      pg_table,
      df,
      append = TRUE,
      row.names = FALSE
    )

  }

  message(sprintf("  ✓ %s rows", nrow(df)))

  invisible(nrow(df))
}

# ----------------------------------------------------------------------------
# Validation helper
# ----------------------------------------------------------------------------

count_rows <- function(con, table_name) {

  DBI::dbGetQuery(
    con,
    sprintf("SELECT COUNT(*) AS n FROM %s", table_name)
  )$n

}

message("✓ Utility functions loaded")

# ----------------------------------------------------------------------------
# Clear PostgreSQL tables
# ----------------------------------------------------------------------------

message("Clearing PostgreSQL tables...")

dbExecute(
  pg_con,
  "
  TRUNCATE TABLE
    growth_phenotypes,
    assemblies,
    genotypes,
    seq_data,
    alt_yjs,
    alt_names,
    yjs_numbers,
    strains,
    conditions,
    projects,
    users,
    pending_growth,
    pending_seqdata,
    pending_altnames,
    pending_strains,
    pending_yjs,
    notifications,
    custom_options
  RESTART IDENTITY CASCADE
  "
)

message("✓ PostgreSQL tables cleared")


# ----------------------------------------------------------------------------
# Migrate main database
# ----------------------------------------------------------------------------

message("")
message("=== Migrating main database ===")

main_order <- c(
  "Strains",
  "Projects",
  "Conditions",
  "YJSnumbers",
  "AltNames",
  "AltYJS",
  "SeqData",
  "Genotypes",
  "Assemblies",
  "GrowthPhenotypes"
)

for (sqlite_table in main_order) {

  pg_table <- main_tables[[sqlite_table]]

  copy_table(
    main_sqlite,
    sqlite_table,
    pg_con,
    pg_table
  )

}

# ----------------------------------------------------------------------------
# Migrate users
# ----------------------------------------------------------------------------

message("")
message("=== Migrating users ===")

copy_table(
  users_sqlite,
  "users",
  pg_con,
  "users"
)

# ----------------------------------------------------------------------------
# Migrate pending tables
# ----------------------------------------------------------------------------

message("")
message("=== Migrating pending tables ===")

for (sqlite_table in names(pending_tables)) {

  pg_table <- pending_tables[[sqlite_table]]

  copy_table(
    pending_sqlite,
    sqlite_table,
    pg_con,
    pg_table
  )

}

# ----------------------------------------------------------------------------
# Reset PostgreSQL sequences
# ----------------------------------------------------------------------------

message("")
message("Resetting PostgreSQL sequences...")

reset_sequence <- function(
  table_name,
  column_name
) {

  query <- sprintf(
    "
    SELECT setval(
      pg_get_serial_sequence('%s', '%s'),
      COALESCE(
        (SELECT MAX(%s) FROM %s),
        1
      ),
      TRUE
    );
    ",
    table_name,
    column_name,
    column_name,
    table_name
  )

  DBI::dbGetQuery(pg_con, query)

}

serial_tables <- list(
  alt_names = "id",
  seq_data = "id_seq_data",
  genotypes = "id_genotype",
  assemblies = "id_assembly",
  growth_phenotypes = "id",
  users = "id",
  pending_yjs = "id",
  pending_strains = "id",
  pending_altnames = "id",
  pending_seqdata = "id",
  pending_growth = "id",
  custom_options = "id",
  notifications = "id"
)

for (tbl in names(serial_tables)) {

  reset_sequence(
    tbl,
    serial_tables[[tbl]]
  )

}

message("✓ Sequences reset")


# ----------------------------------------------------------------------------
# Validation
# ----------------------------------------------------------------------------

message("")
message("=== Migration validation ===")

validate_table <- function(
  sqlite_con,
  sqlite_table,
  pg_table
) {

  sqlite_n <- count_rows(
    sqlite_con,
    sqlite_table
  )

  pg_n <- count_rows(
    pg_con,
    pg_table
  )

  match <- sqlite_n == pg_n

  cat(
    sprintf(
      "%-25s SQLite=%-8s PostgreSQL=%-8s %s\n",
      pg_table,
      sqlite_n,
      pg_n,
      ifelse(match, "OK", "MISMATCH")
    )
  )

  match

}

all_valid <- TRUE

for (sqlite_table in names(main_tables)) {

  all_valid <- all_valid &&
    validate_table(
      main_sqlite,
      sqlite_table,
      main_tables[[sqlite_table]]
    )

}

all_valid <- all_valid &&
  validate_table(
    users_sqlite,
    "users",
    "users"
  )

for (sqlite_table in names(pending_tables)) {

  all_valid <- all_valid &&
    validate_table(
      pending_sqlite,
      sqlite_table,
      pending_tables[[sqlite_table]]
    )

}

# ----------------------------------------------------------------------------
# Cleanup
# ----------------------------------------------------------------------------

DBI::dbDisconnect(main_sqlite)
DBI::dbDisconnect(users_sqlite)
DBI::dbDisconnect(pending_sqlite)
DBI::dbDisconnect(pg_con)

message("")

if (all_valid) {

  message("✅ Migration completed successfully")

} else {

  stop(
    "❌ Migration completed but validation failed"
  )

}

