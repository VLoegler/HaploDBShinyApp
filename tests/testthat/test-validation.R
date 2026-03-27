library(testthat)

# Helper to create mock databases for validation tests
setup_mock_dbs <- function() {
  main_conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(main_conn, "CREATE TABLE YJSnumbers (YJS_NUMBER TEXT PRIMARY KEY)")
  DBI::dbExecute(main_conn, "CREATE TABLE Strains (STRAIN TEXT PRIMARY KEY)")
  DBI::dbExecute(main_conn, "INSERT INTO YJSnumbers VALUES ('YJS0001')")
  DBI::dbExecute(main_conn, "INSERT INTO Strains VALUES ('XTRA_AAA')")

  tmp <- tempfile(fileext = ".sqlite")
  pending_conn <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  ensure_pending_tables(pending_conn)

  list(main = main_conn, pending = pending_conn, tmp = tmp)
}

teardown_mock_dbs <- function(dbs) {
  DBI::dbDisconnect(dbs$main)
  DBI::dbDisconnect(dbs$pending)
  unlink(dbs$tmp)
}

# Helper to build a valid YJS data.frame (box storage by default)
make_yjs_df <- function(...) {
  defaults <- list(
    SAMPLE_NAME = "Test sample",
    SPECIES = NA, MATING_TYPE = NA, PLOIDY = NA,
    GENOTYPE = NA, SPORULATION = NA, EXTERNAL_ORIGIN = NA,
    ECO_ORIGIN = NA, COMMENTS_ORIGIN = NA, PARENTAL_ORIGIN = NA,
    PUBLICATION = NA, STRAINS_GROUP = NA, OLD_BOX = NA,
    BOX_NUMBER = 1, BOX_ROW = NA, BOX_COL = NA,
    PLATE = NA, PLATE_ROW = NA, PLATE_COL = NA,
    NOTES = NA, STOCKED_BY = "testuser", COMMENTS = NA,
    ID_STRAIN = NA, SAMPLE_TYPE = NA, COLLECTION = NA
  )
  overrides <- list(...)
  defaults[names(overrides)] <- overrides
  as.data.frame(defaults, stringsAsFactors = FALSE)
}

# -- YJS validation --

test_that("validate_yjs passes for valid data (box storage)", {
  dbs <- setup_mock_dbs()

  df <- make_yjs_df(
    SPECIES = "Saccharomyces cerevisiae",
    MATING_TYPE = "MATa", PLOIDY = "2n",
    ID_STRAIN = "XTRA_AAA"
  )

  result <- validate_yjs(df, dbs$main, dbs$pending)
  expect_true(result$valid)
  expect_length(result$errors, 0)

  teardown_mock_dbs(dbs)
})

test_that("validate_yjs passes for plate storage only", {
  dbs <- setup_mock_dbs()

  df <- make_yjs_df(
    BOX_NUMBER = NA, PLATE = 1, PLATE_ROW = 3, PLATE_COL = 5
  )

  result <- validate_yjs(df, dbs$main, dbs$pending)
  expect_true(result$valid)

  teardown_mock_dbs(dbs)
})

test_that("validate_yjs passes for both box and plate storage", {
  dbs <- setup_mock_dbs()

  df <- make_yjs_df(
    BOX_NUMBER = 1, PLATE = 2, PLATE_ROW = 1, PLATE_COL = 1
  )

  result <- validate_yjs(df, dbs$main, dbs$pending)
  expect_true(result$valid)

  teardown_mock_dbs(dbs)
})

test_that("validate_yjs fails when neither box nor plate provided", {
  dbs <- setup_mock_dbs()

  df <- make_yjs_df(BOX_NUMBER = NA)

  result <- validate_yjs(df, dbs$main, dbs$pending)
  expect_false(result$valid)
  expect_true(any(grepl("BOX_NUMBER.*PLATE", result$errors)))

  teardown_mock_dbs(dbs)
})

test_that("validate_yjs fails with incomplete plate location", {
  dbs <- setup_mock_dbs()

  df <- make_yjs_df(BOX_NUMBER = NA, PLATE = 1, PLATE_ROW = NA, PLATE_COL = NA)

  result <- validate_yjs(df, dbs$main, dbs$pending)
  expect_false(result$valid)

  teardown_mock_dbs(dbs)
})

test_that("validate_yjs catches missing required fields", {
  dbs <- setup_mock_dbs()

  df <- make_yjs_df(SAMPLE_NAME = NA, STOCKED_BY = NA, BOX_NUMBER = NA)

  result <- validate_yjs(df, dbs$main, dbs$pending)
  expect_false(result$valid)
  expect_true(any(grepl("SAMPLE_NAME", result$errors)))
  expect_true(any(grepl("STOCKED_BY", result$errors)))

  teardown_mock_dbs(dbs)
})

test_that("validate_yjs catches invalid ploidy format", {
  dbs <- setup_mock_dbs()

  df <- make_yjs_df(PLOIDY = "invalid")

  result <- validate_yjs(df, dbs$main, dbs$pending)
  expect_false(result$valid)
  expect_true(any(grepl("PLOIDY", result$errors)))

  teardown_mock_dbs(dbs)
})

test_that("validate_yjs accepts valid ploidy formats", {
  dbs <- setup_mock_dbs()

  for (ploidy in c("1n", "2n", "3n", "10n")) {
    df <- make_yjs_df(PLOIDY = ploidy)
    result <- validate_yjs(df, dbs$main, dbs$pending)
    expect_true(result$valid, info = paste("ploidy:", ploidy))
  }

  teardown_mock_dbs(dbs)
})

test_that("validate_yjs catches non-existent ID_STRAIN", {
  dbs <- setup_mock_dbs()

  df <- make_yjs_df(ID_STRAIN = "NONEXISTENT")

  result <- validate_yjs(df, dbs$main, dbs$pending)
  expect_false(result$valid)
  expect_true(any(grepl("ID_STRAIN.*not found", result$errors)))

  teardown_mock_dbs(dbs)
})

test_that("validate_yjs accepts ID_STRAIN from pending strains", {
  dbs <- setup_mock_dbs()

  DBI::dbExecute(dbs$pending,
    "INSERT INTO pending_strains (STRAIN, original_name, submitted_by) VALUES ('XTRA_NEW', 'test', 'user')")

  df <- make_yjs_df(ID_STRAIN = "XTRA_NEW")

  result <- validate_yjs(df, dbs$main, dbs$pending)
  expect_true(result$valid)

  teardown_mock_dbs(dbs)
})

test_that("validate_yjs validates multiple rows independently", {
  dbs <- setup_mock_dbs()

  df <- data.frame(
    SAMPLE_NAME = c("Good", NA),
    SPECIES = c(NA, NA), MATING_TYPE = c(NA, NA), PLOIDY = c(NA, NA),
    GENOTYPE = c(NA, NA), SPORULATION = c(NA, NA),
    EXTERNAL_ORIGIN = c(NA, NA), ECO_ORIGIN = c(NA, NA),
    COMMENTS_ORIGIN = c(NA, NA), PARENTAL_ORIGIN = c(NA, NA),
    PUBLICATION = c(NA, NA), STRAINS_GROUP = c(NA, NA),
    OLD_BOX = c(NA, NA), BOX_NUMBER = c(1, 2),
    BOX_ROW = c(NA, NA), BOX_COL = c(NA, NA),
    PLATE = c(NA, NA), PLATE_ROW = c(NA, NA), PLATE_COL = c(NA, NA),
    NOTES = c(NA, NA), STOCKED_BY = c("user", "user"),
    COMMENTS = c(NA, NA), ID_STRAIN = c(NA, NA),
    SAMPLE_TYPE = c(NA, NA), COLLECTION = c(NA, NA),
    stringsAsFactors = FALSE
  )

  result <- validate_yjs(df, dbs$main, dbs$pending)
  expect_false(result$valid)
  expect_true(any(grepl("Row 2.*SAMPLE_NAME", result$errors)))
  expect_false(any(grepl("Row 1", result$errors)))

  teardown_mock_dbs(dbs)
})

# -- Strain validation --

test_that("validate_strains passes for valid data", {
  dbs <- setup_mock_dbs()

  df <- data.frame(
    STRAIN = "XTRA_NEW",
    stringsAsFactors = FALSE
  )

  result <- validate_strains(df, dbs$main, dbs$pending)
  expect_true(result$valid)

  teardown_mock_dbs(dbs)
})

test_that("validate_strains catches duplicate strain in main DB", {
  dbs <- setup_mock_dbs()

  df <- data.frame(
    STRAIN = "XTRA_AAA",
    stringsAsFactors = FALSE
  )

  result <- validate_strains(df, dbs$main, dbs$pending)
  expect_false(result$valid)
  expect_true(any(grepl("already exists", result$errors)))

  teardown_mock_dbs(dbs)
})

test_that("validate_strains catches duplicate strain in pending DB", {
  dbs <- setup_mock_dbs()

  DBI::dbExecute(dbs$pending,
    "INSERT INTO pending_strains (STRAIN, original_name, submitted_by) VALUES ('XTRA_BBB', 'test', 'user')")

  df <- data.frame(
    STRAIN = "XTRA_BBB",
    stringsAsFactors = FALSE
  )

  result <- validate_strains(df, dbs$main, dbs$pending)
  expect_false(result$valid)
  expect_true(any(grepl("already exists", result$errors)))

  teardown_mock_dbs(dbs)
})
