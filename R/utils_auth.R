#' Authentication Utilities
#'
#' Helpers for user authentication, password hashing, and role checking.
#' Uses sodium for password hashing and shinyauthr for session management.

#' Verify user credentials against the database
#'
#' @param username Character string
#' @param password Character string (plaintext)
#' @param users_conn SQLite connection for users database
#' @return A single-row data.frame with id, username, role if valid; NULL otherwise
check_credentials <- function(username, password, users_conn) {
  user <- DBI::dbGetQuery(
    users_conn,
    "SELECT id, username, password_hash, role FROM users WHERE username = ?",
    params = list(username)
  )

  if (nrow(user) == 0) return(NULL)

  if (sodium::password_verify(user$password_hash, as.character(password))) {
    user[, c("id", "username", "role")]
  } else {
    NULL
  }
}

#' Check if a user has admin role
#'
#' @param user_info Reactive value or data.frame with role column
#' @return Logical
is_admin <- function(user_info) {
  !is.null(user_info) && nrow(user_info) > 0 && user_info$role == "admin"
}

#' Hash a password for storage
#'
#' @param password Character string (plaintext)
#' @return Character string (hashed)
hash_password <- function(password) {
  sodium::password_store(as.character(password))
}

# --- Session token store (survives page refresh within same R process) ---

.session_tokens <- new.env(parent = emptyenv())

generate_session_token <- function() {
  paste0(sample(c(letters, LETTERS, 0:9), 64, replace = TRUE), collapse = "")
}

store_session_token <- function(token, user_info) {
  .session_tokens[[token]] <- user_info
}

retrieve_session_token <- function(token) {
  if (is.null(token) || token == "" || !exists(token, envir = .session_tokens)) return(NULL)
  .session_tokens[[token]]
}

remove_session_token <- function(token) {
  if (!is.null(token) && exists(token, envir = .session_tokens)) {
    rm(list = token, envir = .session_tokens)
  }
}
