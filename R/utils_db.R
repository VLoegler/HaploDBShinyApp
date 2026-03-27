#' Database Utilities
#'
#' Helpers for SQLite connections (main DB, pending submissions, users).
#' All database interactions go through parameterized queries.

#' Open a SQLite connection with WAL mode enabled
#'
#' @param path Path to the SQLite file
#' @return A DBI connection object
create_db_conn <- function(path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  conn <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(conn, "PRAGMA journal_mode=WAL")
  DBI::dbExecute(conn, "PRAGMA busy_timeout=5000")
  conn
}

#' Ensure all pending tables exist in SQLite
#'
#' Creates pending_yjs, pending_strains, and deferred tables.
#'
#' @param sqlite_conn SQLite DBI connection
ensure_pending_tables <- function(sqlite_conn) {
  DBI::dbExecute(sqlite_conn, "
    CREATE TABLE IF NOT EXISTS pending_yjs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      YJS_NUMBER TEXT NOT NULL,
      SAMPLE_NAME TEXT,
      SPECIES TEXT,
      MATING_TYPE TEXT,
      PLOIDY TEXT,
      GENOTYPE TEXT,
      SPORULATION TEXT,
      EXTERNAL_ORIGIN TEXT,
      ECO_ORIGIN TEXT,
      COMMENTS_ORIGIN TEXT,
      PARENTAL_ORIGIN TEXT,
      PUBLICATION TEXT,
      STRAINS_GROUP TEXT,
      OLD_BOX INTEGER,
      BOX_NUMBER INTEGER,
      BOX_ROW INTEGER,
      BOX_COL INTEGER,
      PLATE INTEGER,
      PLATE_ROW INTEGER,
      PLATE_COL INTEGER,
      NOTES TEXT,
      STOCKED_BY TEXT,
      COMMENTS TEXT,
      ID_STRAIN TEXT,
      SAMPLE_TYPE TEXT,
      COLLECTION TEXT,
      submitted_by TEXT NOT NULL,
      submitted_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  ")

  DBI::dbExecute(sqlite_conn, "
    CREATE TABLE IF NOT EXISTS pending_strains (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      STRAIN TEXT NOT NULL,
      ISOLATION TEXT,
      ECO_ORIGIN TEXT,
      GEO_ORIGIN TEXT,
      CONTINENT TEXT,
      COUNTRY TEXT,
      CLADE TEXT,
      SRR_ID TEXT,
      SPECIES TEXT,
      original_name TEXT NOT NULL,
      submitted_by TEXT NOT NULL,
      submitted_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  ")

  DBI::dbExecute(sqlite_conn, "
    CREATE TABLE IF NOT EXISTS pending_altnames (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      STRAIN TEXT NOT NULL,
      ALT_NAME TEXT NOT NULL,
      submitted_by TEXT NOT NULL,
      submitted_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  ")

  DBI::dbExecute(sqlite_conn, "
    CREATE TABLE IF NOT EXISTS pending_seqdata (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      PATH TEXT NOT NULL,
      METHOD TEXT,
      MOLECULE TEXT,
      FILETYPE TEXT,
      ORIGIN_LAB TEXT,
      YJS_NUMBER TEXT,
      ID_Project TEXT,
      COMMENT TEXT,
      submitted_by TEXT NOT NULL,
      submitted_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  ")

  DBI::dbExecute(sqlite_conn, "
    CREATE TABLE IF NOT EXISTS pending_growth (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      YJS_NUMBER TEXT NOT NULL,
      DATE TEXT,
      USER TEXT,
      COND TEXT NOT NULL,
      REF_COND TEXT NOT NULL,
      SIZE REAL,
      SIZE_REF REAL,
      GROWTHRATIO REAL,
      TIMEPOINT TEXT,
      ID_Project TEXT,
      submitted_by TEXT NOT NULL,
      submitted_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  ")

  DBI::dbExecute(sqlite_conn, "
    CREATE TABLE IF NOT EXISTS pending_rnaseq (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      YJS_NUMBER TEXT NOT NULL,
      COND TEXT NOT NULL,
      GENE TEXT NOT NULL,
      COUNT REAL,
      TPM REAL,
      ID_Project TEXT,
      submitted_by TEXT NOT NULL,
      submitted_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  ")

  DBI::dbExecute(sqlite_conn, "
    CREATE TABLE IF NOT EXISTS pending_proteomics (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      YJS_NUMBER TEXT NOT NULL,
      COND TEXT NOT NULL,
      PROTEIN TEXT NOT NULL,
      PROT_ABUNDANCE REAL,
      ID_Project TEXT,
      submitted_by TEXT NOT NULL,
      submitted_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  ")

  DBI::dbExecute(sqlite_conn, "
    CREATE TABLE IF NOT EXISTS custom_options (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      field_name TEXT NOT NULL,
      value TEXT NOT NULL,
      UNIQUE(field_name, value)
    )
  ")

  DBI::dbExecute(sqlite_conn, "
    CREATE TABLE IF NOT EXISTS notifications (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT NOT NULL,
      entry_type TEXT NOT NULL,
      entry_name TEXT,
      assigned_number TEXT,
      status TEXT NOT NULL,
      reviewer TEXT NOT NULL,
      created_at TEXT NOT NULL DEFAULT (datetime('now')),
      is_read INTEGER NOT NULL DEFAULT 0,
      box TEXT,
      box_row TEXT,
      box_col TEXT,
      plate TEXT,
      plate_row TEXT,
      plate_col TEXT
    )
  ")
}

#' Ensure the users table exists in the users SQLite database
#'
#' @param users_conn SQLite DBI connection for users database
ensure_users_table <- function(users_conn) {
  DBI::dbExecute(users_conn, "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL,
      role TEXT NOT NULL DEFAULT 'basic',
      created_at TEXT DEFAULT (datetime('now'))
    )
  ")
}

#' Seed default admin account if no admin exists
#'
#' @param users_conn SQLite DBI connection for users database
#' @param admin_config List with username and password from config.yml
seed_default_admin <- function(users_conn, admin_config) {
  admin_count <- DBI::dbGetQuery(
    users_conn,
    "SELECT COUNT(*) AS n FROM users WHERE role = 'admin'"
  )$n

  if (admin_count == 0) {
    hashed <- sodium::password_store(as.character(admin_config$password))
    DBI::dbExecute(
      users_conn,
      "INSERT INTO users (username, password_hash, role) VALUES (?, ?, 'admin')",
      params = list(admin_config$username, hashed)
    )
    message("Default admin account created: ", admin_config$username)
  }
}

#' Get choices for a dynamic field (species, ploidy, mating_type)
#'
#' Merges distinct values from the main DB with custom options from SQLite.
#' @param sqlite_conn SQLite connection (pending DB)
#' @param main_conn Main DB connection
#' @param field_name One of "species", "ploidy", "mating_type"
#' @return Character vector of unique values
get_field_choices <- function(sqlite_conn, main_conn, field_name) {
  custom <- DBI::dbGetQuery(
    sqlite_conn,
    "SELECT value FROM custom_options WHERE field_name = ?",
    params = list(field_name)
  )$value

  if (field_name == "species") {
    db_vals <- unique(c(
      DBI::dbGetQuery(
        main_conn,
        "SELECT DISTINCT SPECIES FROM YJSnumbers
         WHERE SPECIES IS NOT NULL AND SPECIES != ''"
      )$SPECIES,
      DBI::dbGetQuery(
        main_conn,
        "SELECT DISTINCT SPECIES FROM Strains
         WHERE SPECIES IS NOT NULL AND SPECIES != ''"
      )$SPECIES
    ))
  } else if (field_name == "ploidy") {
    db_vals <- DBI::dbGetQuery(
      main_conn,
      "SELECT DISTINCT PLOIDY FROM YJSnumbers
       WHERE PLOIDY IS NOT NULL AND PLOIDY != ''"
    )$PLOIDY
  } else if (field_name == "mating_type") {
    db_vals <- DBI::dbGetQuery(
      main_conn,
      "SELECT DISTINCT MATING_TYPE FROM YJSnumbers
       WHERE MATING_TYPE IS NOT NULL AND MATING_TYPE != ''"
    )$MATING_TYPE
  } else if (field_name == "collection") {
    db_vals <- DBI::dbGetQuery(
      main_conn,
      "SELECT DISTINCT COLLECTION FROM YJSnumbers
       WHERE COLLECTION IS NOT NULL AND COLLECTION != ''"
    )$COLLECTION
  } else {
    db_vals <- character(0)
  }

  sort(unique(c(db_vals, custom)))
}

#' Save a custom option value for future use
#'
#' @param sqlite_conn SQLite connection
#' @param field_name Field name (e.g., "species")
#' @param value The new value to save
save_custom_option <- function(sqlite_conn, field_name, value) {
  DBI::dbExecute(
    sqlite_conn,
    "INSERT OR IGNORE INTO custom_options (field_name, value)
     VALUES (?, ?)",
    params = list(field_name, value)
  )
}

#' Get the next YJS number(s) based on current max in the DB
#'
#' @param main_conn Main DB connection
#' @param count How many numbers to generate
#' @return Character vector of YJS numbers (e.g., "YJS0201")
get_next_yjs_number <- function(main_conn, count = 1) {
  all_yjs <- DBI::dbGetQuery(
    main_conn, "SELECT YJS_NUMBER FROM YJSnumbers"
  )$YJS_NUMBER
  nums <- suppressWarnings(as.integer(gsub("^YJS", "", all_yjs)))
  max_num <- if (length(nums) == 0 || all(is.na(nums))) {
    0L
  } else {
    max(nums, na.rm = TRUE)
  }
  paste0("YJS", seq(max_num + 1, max_num + count))
}

#' Create a notification for a user about their submission
#'
#' @param sqlite_conn SQLite connection
#' @param username The submitter's username
#' @param entry_type "YJS Sample" or "Strain"
#' @param entry_name Sample name or strain name
#' @param assigned_number Assigned YJS/XTRA number (or NA)
#' @param status "approved" or "rejected"
#' @param reviewer Admin username who reviewed
create_notification <- function(sqlite_conn, username, entry_type,
                                entry_name, assigned_number,
                                status, reviewer,
                                box = NA_character_,
                                box_row = NA_character_,
                                box_col = NA_character_,
                                plate = NA_character_,
                                plate_row = NA_character_,
                                plate_col = NA_character_) {
  DBI::dbExecute(
    sqlite_conn,
    "INSERT INTO notifications
       (username, entry_type, entry_name, assigned_number,
        status, reviewer, created_at,
        box, box_row, box_col, plate, plate_row, plate_col)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      username, entry_type, entry_name,
      if (is.null(assigned_number)) NA_character_
      else assigned_number,
      status, reviewer,
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      as.character(box), as.character(box_row),
      as.character(box_col), as.character(plate),
      as.character(plate_row), as.character(plate_col)
    )
  )
}
