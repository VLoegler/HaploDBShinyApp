library(testthat)

test_that("get_next_yjs_number creates correct sequence", {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(conn, "CREATE TABLE YJSnumbers (YJS_NUMBER TEXT PRIMARY KEY)")
  DBI::dbExecute(conn, "INSERT INTO YJSnumbers VALUES ('YJS8349')")

  result <- get_next_yjs_number(conn, 3)
  expect_equal(result, c("YJS8350", "YJS8351", "YJS8352"))

  DBI::dbDisconnect(conn)
})

test_that("get_next_yjs_number works with empty table", {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(conn, "CREATE TABLE YJSnumbers (YJS_NUMBER TEXT PRIMARY KEY)")

  result <- get_next_yjs_number(conn, 1)
  expect_equal(result, "YJS1")

  DBI::dbDisconnect(conn)
})

test_that("get_next_yjs_number works with large numbers", {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(conn, "CREATE TABLE YJSnumbers (YJS_NUMBER TEXT PRIMARY KEY)")
  DBI::dbExecute(conn, "INSERT INTO YJSnumbers VALUES ('YJS9999')")

  result <- get_next_yjs_number(conn, 2)
  expect_equal(result, c("YJS10000", "YJS10001"))

  DBI::dbDisconnect(conn)
})

test_that("na_if_empty converts empty strings to NA", {
  expect_true(is.na(na_if_empty("")))
  expect_true(is.na(na_if_empty("  ")))
  expect_true(is.na(na_if_empty(NULL)))
  expect_equal(na_if_empty("hello"), "hello")
  expect_equal(na_if_empty(42), 42)
})

test_that("na_to_empty converts NA to empty string", {
  expect_equal(na_to_empty(NA), "")
  expect_equal(na_to_empty(NULL), "")
  expect_equal(na_to_empty("hello"), "hello")
  expect_equal(na_to_empty(42), "42")
})

test_that("next_xtra_names increments correctly", {
  tmp <- tempfile(fileext = ".sqlite")
  pending_conn <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  ensure_pending_tables(pending_conn)

  main_conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(main_conn, "CREATE TABLE Strains (STRAIN TEXT PRIMARY KEY)")
  DBI::dbExecute(main_conn, "INSERT INTO Strains VALUES ('XTRA_AAC')")

  result <- next_xtra_names(main_conn, pending_conn, 3)
  expect_equal(result, c("XTRA_AAD", "XTRA_AAE", "XTRA_AAF"))

  DBI::dbDisconnect(pending_conn)
  DBI::dbDisconnect(main_conn)
  unlink(tmp)
})

test_that("next_xtra_names handles Z rollover", {
  main_conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(main_conn, "CREATE TABLE Strains (STRAIN TEXT PRIMARY KEY)")
  DBI::dbExecute(main_conn, "INSERT INTO Strains VALUES ('XTRA_AAZ')")

  tmp <- tempfile(fileext = ".sqlite")
  pending_conn <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  ensure_pending_tables(pending_conn)

  result <- next_xtra_names(main_conn, pending_conn, 2)
  expect_equal(result, c("XTRA_ABA", "XTRA_ABB"))

  DBI::dbDisconnect(pending_conn)
  DBI::dbDisconnect(main_conn)
  unlink(tmp)
})

test_that("next_xtra_names handles double rollover", {
  main_conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(main_conn, "CREATE TABLE Strains (STRAIN TEXT PRIMARY KEY)")
  DBI::dbExecute(main_conn, "INSERT INTO Strains VALUES ('XTRA_AZZ')")

  tmp <- tempfile(fileext = ".sqlite")
  pending_conn <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  ensure_pending_tables(pending_conn)

  result <- next_xtra_names(main_conn, pending_conn, 1)
  expect_equal(result, "XTRA_BAA")

  DBI::dbDisconnect(pending_conn)
  DBI::dbDisconnect(main_conn)
  unlink(tmp)
})

test_that("next_xtra_names starts at AAA when no strains exist", {
  main_conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(main_conn, "CREATE TABLE Strains (STRAIN TEXT PRIMARY KEY)")

  tmp <- tempfile(fileext = ".sqlite")
  pending_conn <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  ensure_pending_tables(pending_conn)

  result <- next_xtra_names(main_conn, pending_conn, 2)
  expect_equal(result, c("XTRA_AAA", "XTRA_AAB"))

  DBI::dbDisconnect(pending_conn)
  DBI::dbDisconnect(main_conn)
  unlink(tmp)
})

test_that("next_xtra_names considers pending strains", {
  main_conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(main_conn, "CREATE TABLE Strains (STRAIN TEXT PRIMARY KEY)")
  DBI::dbExecute(main_conn, "INSERT INTO Strains VALUES ('XTRA_AAA')")

  tmp <- tempfile(fileext = ".sqlite")
  pending_conn <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  ensure_pending_tables(pending_conn)
  DBI::dbExecute(pending_conn,
    "INSERT INTO pending_strains (STRAIN, original_name, submitted_by) VALUES ('XTRA_AAD', 'test', 'user')")

  result <- next_xtra_names(main_conn, pending_conn, 1)
  expect_equal(result, "XTRA_AAE")

  DBI::dbDisconnect(pending_conn)
  DBI::dbDisconnect(main_conn)
  unlink(tmp)
})

test_that("next_xtra_names considers extra_names argument", {
  main_conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(main_conn, "CREATE TABLE Strains (STRAIN TEXT PRIMARY KEY)")

  tmp <- tempfile(fileext = ".sqlite")
  pending_conn <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  ensure_pending_tables(pending_conn)

  result <- next_xtra_names(main_conn, pending_conn, 1, extra_names = "XTRA_AAC")
  expect_equal(result, "XTRA_AAD")

  DBI::dbDisconnect(pending_conn)
  DBI::dbDisconnect(main_conn)
  unlink(tmp)
})
