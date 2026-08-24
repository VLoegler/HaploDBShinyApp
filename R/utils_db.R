#' Database Utilities
#'
#' Helpers for Database connections (main DB, pending submissions, users).
#' All database interactions go through parameterized queries.

#' Ensure the users table exists in the PostgreSQL database
#'
#' @param db_conn Database DBI connection
#' @return The connection invisibly
ensure_users_table <- function(db_conn) {
  if (!DBI::dbExistsTable(db_conn, "users")) {
    DBI::dbExecute(
      db_conn,
      "
      CREATE TABLE IF NOT EXISTS users (
        id BIGSERIAL PRIMARY KEY,
        username TEXT UNIQUE NOT NULL,
        password_hash TEXT NOT NULL,
        role TEXT NOT NULL DEFAULT 'basic',
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
      "
    )
  }

  invisible(db_conn)
}

#' Seed the configured admin account from environment variables
#'
#' Reads MASTERADMIN and MASTERPWD from the environment (or .env when loaded).
#' Creates a new admin user when the configured username does not exist.
#' Existing accounts are left unchanged so application restarts cannot reset
#' passwords or alter roles.
#'
#' @param db_conn Database DBI connection for users table
#' @param admin_config Optional list with username and password values
seed_default_admin <- function(db_conn, admin_config = NULL) {
  ensure_users_table(db_conn)

  if (is.null(admin_config)) {
    admin_config <- list(
      username = Sys.getenv("MASTERADMIN", unset = ""),
      password = Sys.getenv("MASTERPWD", unset = "")
    )
  }

  username <- as.character(admin_config$username)
  password <- as.character(admin_config$password)

  if (!nzchar(username) || !nzchar(password)) {
    message("Skipping admin bootstrap: MASTERADMIN and MASTERPWD must be set")
    return(invisible(NULL))
  }

  hashed <- sodium::password_store(password)

  existing <- DBI::dbGetQuery(
    db_conn,
    "SELECT id FROM users WHERE username = $1",
    params = list(username)
  )

  if (nrow(existing) == 0) {
    DBI::dbExecute(
      db_conn,
      "INSERT INTO users (username, password_hash, role) VALUES ($1, $2, 'admin')",
      params = list(username, hashed)
    )
    message("Default admin account created: ", username)
  } else {
    message("Default admin account already exists; leaving it unchanged: ", username)
  }

  invisible(NULL)
}

#' Get choices for a dynamic field (species, ploidy, mating_type)
#'
#' Merges distinct values from the main DB with custom options from PostgreSQL
#' @param db_conn Database connection
#' @param field_name One of "species", "ploidy", "mating_type"
#' @return Character vector of unique values
get_field_choices <- function(db_conn, field_name) {
  custom <- DBI::dbGetQuery(
    db_conn,
    "SELECT value FROM custom_options WHERE field_name = $1",
    params = list(field_name)
  )$value

  if (field_name == "species") {
    db_vals <- unique(c(
      DBI::dbGetQuery(
        db_conn,
        "SELECT DISTINCT species FROM yjs_numbers
         WHERE species IS NOT NULL AND species != ''"
      )$species,
      DBI::dbGetQuery(
        db_conn,
        "SELECT DISTINCT species FROM strains
         WHERE species IS NOT NULL AND species != ''"
      )$species
    ))
  } else if (field_name == "ploidy") {
    db_vals <- DBI::dbGetQuery(
      db_conn,
      "SELECT DISTINCT ploidy FROM yjs_numbers
       WHERE ploidy IS NOT NULL AND ploidy != ''"
    )$ploidy
  } else if (field_name == "mating_type") {
    db_vals <- DBI::dbGetQuery(
      db_conn,
      "SELECT DISTINCT mating_type FROM yjs_numbers
       WHERE mating_type IS NOT NULL AND mating_type != ''"
    )$mating_type
  } else if (field_name == "collection") {
    db_vals <- DBI::dbGetQuery(
      db_conn,
      "SELECT DISTINCT collection FROM yjs_numbers
       WHERE collection IS NOT NULL AND collection != ''"
    )$collection
  } else {
    db_vals <- character(0)
  }

  sort(unique(c(db_vals, custom)))
}

#' Save a custom option value for future use
#'
#' @param db_conn Database connection
#' @param field_name Field name (e.g., "species")
#' @param value The new value to save
save_custom_option <- function(db_conn, field_name, value) {
  DBI::dbExecute(
    db_conn,
    "
    INSERT INTO custom_options (field_name, value)
    VALUES ($1, $2)
    ON CONFLICT (field_name, value) DO NOTHING
    ",
    params = list(field_name, value)
  )
}

#' Get the next YJS number(s) based on current max in the DB
#'
#' @param db_conn Database connection
#' @param count How many numbers to generate
#' @return Character vector of YJS numbers (e.g., "YJS0201")
get_next_yjs_number <- function(db_conn, count = 1) {
  all_yjs <- DBI::dbGetQuery(
    db_conn, "SELECT yjs_number FROM yjs_numbers"
  )$yjs_number
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
#' @param db_conn Database connection
#' @param username The submitter's username
#' @param entry_type "YJS Sample" or "Strain"
#' @param entry_name Sample name or strain name
#' @param assigned_number Assigned YJS/XTRA number (or NA)
#' @param status "approved" or "rejected"
#' @param reviewer Admin username who reviewed
create_notification <- function(db_conn, username, entry_type,
                                entry_name, assigned_number,
                                status, reviewer,
                                box = NA_character_,
                                box_row = NA_character_,
                                box_col = NA_character_,
                                plate = NA_character_,
                                plate_row = NA_character_,
                                plate_col = NA_character_) {
  DBI::dbExecute(
    db_conn,
    "INSERT INTO notifications
       (username, entry_type, entry_name, assigned_number,
        status, reviewer, created_at,
        box, box_row, box_col, plate, plate_row, plate_col)
     VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)",
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
