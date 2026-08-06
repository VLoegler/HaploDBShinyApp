library(testthat)

create_pg_test_conn <- function() {
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    testthat::skip("RPostgres is not installed")
  }

  if (file.exists(".env")) {
    dotenv::load_dot_env(".env")
  }

  if (!nzchar(Sys.getenv("DB_HOST")) || !nzchar(Sys.getenv("DB_NAME")) ||
      !nzchar(Sys.getenv("DB_USER")) || !nzchar(Sys.getenv("DB_PASSWORD"))) {
    testthat::skip("PostgreSQL connection details are not configured")
  }

  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT", "5432")),
    sslmode = "require"
  )

  DBI::dbExecute(conn, "CREATE TEMP TABLE users (id BIGSERIAL PRIMARY KEY, username TEXT UNIQUE NOT NULL, password_hash TEXT NOT NULL, role TEXT NOT NULL DEFAULT 'basic', created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)")
  conn
}

test_that("is_admin returns TRUE for admin user", {
  user <- data.frame(id = 1, username = "admin", role = "admin",
                     stringsAsFactors = FALSE)
  expect_true(is_admin(user))
})

test_that("is_admin returns FALSE for basic user", {
  user <- data.frame(id = 2, username = "user1", role = "basic",
                     stringsAsFactors = FALSE)
  expect_false(is_admin(user))
})

test_that("is_admin returns FALSE for NULL", {
  expect_false(is_admin(NULL))
})

test_that("is_admin returns FALSE for empty data.frame", {
  expect_false(is_admin(data.frame()))
})

test_that("hash_password produces a verifiable hash", {
  pw <- "testpassword123"
  hashed <- hash_password(pw)
  expect_true(sodium::password_verify(hashed, as.character(pw)))
})

test_that("hash_password rejects wrong password", {
  pw <- "correct"
  hashed <- hash_password(pw)
  expect_false(sodium::password_verify(hashed, as.character("wrong")))
})

test_that("check_credentials returns NULL for non-existent user", {
  conn <- create_pg_test_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  result <- check_credentials("nobody", "pass", conn)
  expect_null(result)
})

test_that("check_credentials returns user info for valid credentials", {
  conn <- create_pg_test_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  hashed <- hash_password("secret")
  DBI::dbExecute(conn,
    "INSERT INTO users (username, password_hash, role) VALUES ($1, $2, $3)",
    params = list("testuser", hashed, "admin")
  )

  result <- check_credentials("testuser", "secret", conn)
  expect_false(is.null(result))
  expect_equal(result$username, "testuser")
  expect_equal(result$role, "admin")
})

test_that("check_credentials returns NULL for wrong password", {
  conn <- create_pg_test_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  hashed <- hash_password("correct")
  DBI::dbExecute(conn,
    "INSERT INTO users (username, password_hash, role) VALUES ($1, $2, $3)",
    params = list("testuser", hashed, "basic")
  )

  result <- check_credentials("testuser", "wrong", conn)
  expect_null(result)
})

test_that("session token store and retrieve works", {
  token <- generate_session_token()
  user_info <- data.frame(id = 1, username = "test", role = "basic",
                          stringsAsFactors = FALSE)

  expect_null(retrieve_session_token(token))

  store_session_token(token, user_info)
  retrieved <- retrieve_session_token(token)
  expect_equal(retrieved$username, "test")

  remove_session_token(token)
  expect_null(retrieve_session_token(token))
})

test_that("retrieve_session_token handles edge cases", {
  expect_null(retrieve_session_token(NULL))
  expect_null(retrieve_session_token(""))
  expect_null(retrieve_session_token("nonexistent_token"))
})

test_that("seed_default_admin creates admin when none exists", {
  conn <- create_pg_test_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_users_table(conn)

  seed_default_admin(conn, list(username = "myadmin", password = "mypass"))

  users <- DBI::dbGetQuery(conn, "SELECT username FROM users WHERE role = 'admin'")
  expect_equal(nrow(users), 1)
  expect_equal(users$username, "myadmin")
})

test_that("seed_default_admin updates the configured admin when it already exists", {
  conn <- create_pg_test_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_users_table(conn)

  seed_default_admin(conn, list(username = "admin1", password = "pass1"))
  seed_default_admin(conn, list(username = "admin1", password = "pass2"))

  users <- DBI::dbGetQuery(conn, "SELECT username, role FROM users WHERE username = 'admin1'")
  expect_equal(nrow(users), 1)
  expect_equal(users$role, "admin")
})
