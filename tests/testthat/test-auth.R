library(testthat)

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
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(conn, "
    CREATE TABLE users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL,
      role TEXT NOT NULL DEFAULT 'basic'
    )
  ")

  result <- check_credentials("nobody", "pass", conn)
  expect_null(result)

  DBI::dbDisconnect(conn)
})

test_that("check_credentials returns user info for valid credentials", {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(conn, "
    CREATE TABLE users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL,
      role TEXT NOT NULL DEFAULT 'basic'
    )
  ")

  hashed <- hash_password("secret")
  DBI::dbExecute(conn,
    "INSERT INTO users (username, password_hash, role) VALUES (?, ?, ?)",
    params = list("testuser", hashed, "admin")
  )

  result <- check_credentials("testuser", "secret", conn)
  expect_false(is.null(result))
  expect_equal(result$username, "testuser")
  expect_equal(result$role, "admin")

  DBI::dbDisconnect(conn)
})

test_that("check_credentials returns NULL for wrong password", {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(conn, "
    CREATE TABLE users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL,
      role TEXT NOT NULL DEFAULT 'basic'
    )
  ")

  hashed <- hash_password("correct")
  DBI::dbExecute(conn,
    "INSERT INTO users (username, password_hash, role) VALUES (?, ?, ?)",
    params = list("testuser", hashed, "basic")
  )

  result <- check_credentials("testuser", "wrong", conn)
  expect_null(result)

  DBI::dbDisconnect(conn)
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
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  ensure_users_table(conn)

  seed_default_admin(conn, list(username = "myadmin", password = "mypass"))

  users <- DBI::dbGetQuery(conn, "SELECT * FROM users WHERE role = 'admin'")
  expect_equal(nrow(users), 1)
  expect_equal(users$username, "myadmin")

  DBI::dbDisconnect(conn)
})

test_that("seed_default_admin skips when admin already exists", {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  ensure_users_table(conn)

  seed_default_admin(conn, list(username = "admin1", password = "pass1"))
  seed_default_admin(conn, list(username = "admin2", password = "pass2"))

  users <- DBI::dbGetQuery(conn, "SELECT * FROM users WHERE role = 'admin'")
  expect_equal(nrow(users), 1)
  expect_equal(users$username, "admin1")

  DBI::dbDisconnect(conn)
})
