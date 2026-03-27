#!/usr/bin/env Rscript
# One-off migration script: transforms demo haplodb.sqlite to the new schema.
# Changes:
#   - Rename ROW -> BOX_ROW, COL -> BOX_COL
#   - Add PLATE, PLATE_ROW, PLATE_COL, SAMPLE_TYPE, COLLECTION columns
#   - Make BOX_NUMBER nullable, add CHECK constraint
#   - Add proper FK constraints and indexes

library(DBI)
library(RSQLite)

db_path <- file.path("data", "haploDB", "haplodb.sqlite")
stopifnot(file.exists(db_path))

conn <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(conn), add = TRUE)

dbExecute(conn, "PRAGMA foreign_keys = OFF")

dbExecute(conn, "BEGIN TRANSACTION")

# Recreate YJSnumbers with new schema
dbExecute(conn, "
  CREATE TABLE YJSnumbers_new (
    YJS_NUMBER varchar(50) NOT NULL PRIMARY KEY,
    SAMPLE_NAME text DEFAULT NULL,
    SPECIES text DEFAULT NULL,
    MATING_TYPE text DEFAULT NULL,
    PLOIDY text DEFAULT NULL,
    GENOTYPE text DEFAULT NULL,
    SPORULATION text DEFAULT NULL,
    EXTERNAL_ORIGIN text DEFAULT NULL,
    ECO_ORIGIN text DEFAULT NULL,
    COMMENTS_ORIGIN text DEFAULT NULL,
    PARENTAL_ORIGIN text DEFAULT NULL,
    PUBLICATION text DEFAULT NULL,
    STRAINS_GROUP text DEFAULT NULL,
    OLD_BOX integer DEFAULT NULL,
    BOX_NUMBER integer DEFAULT NULL,
    BOX_ROW integer DEFAULT NULL,
    BOX_COL integer DEFAULT NULL,
    PLATE integer DEFAULT NULL,
    PLATE_ROW integer DEFAULT NULL,
    PLATE_COL integer DEFAULT NULL,
    NOTES text DEFAULT NULL,
    STOCKED_BY text DEFAULT NULL,
    COMMENTS text DEFAULT NULL,
    ID_STRAIN varchar(50) DEFAULT NULL,
    SAMPLE_TYPE tinytext DEFAULT NULL,
    COLLECTION tinytext DEFAULT NULL,
    CONSTRAINT YJSnumbers_ibfk_1 FOREIGN KEY (ID_STRAIN) REFERENCES Strains (STRAIN),
    CHECK (
      BOX_NUMBER IS NOT NULL
      OR
      (PLATE IS NOT NULL AND PLATE_ROW IS NOT NULL AND PLATE_COL IS NOT NULL)
    )
  )
")

dbExecute(conn, "
  INSERT INTO YJSnumbers_new (
    YJS_NUMBER, SAMPLE_NAME, SPECIES, MATING_TYPE, PLOIDY, GENOTYPE,
    SPORULATION, EXTERNAL_ORIGIN, ECO_ORIGIN, COMMENTS_ORIGIN,
    PARENTAL_ORIGIN, PUBLICATION, STRAINS_GROUP, OLD_BOX, BOX_NUMBER,
    BOX_ROW, BOX_COL, NOTES, STOCKED_BY, COMMENTS, ID_STRAIN
  )
  SELECT
    YJS_NUMBER, SAMPLE_NAME, SPECIES, MATING_TYPE, PLOIDY, GENOTYPE,
    SPORULATION, EXTERNAL_ORIGIN, ECO_ORIGIN, COMMENTS_ORIGIN,
    PARENTAL_ORIGIN, PUBLICATION, STRAINS_GROUP, OLD_BOX, BOX_NUMBER,
    ROW, COL, NOTES, STOCKED_BY, COMMENTS, ID_STRAIN
  FROM YJSnumbers
")

dbExecute(conn, "DROP TABLE YJSnumbers")
dbExecute(conn, "ALTER TABLE YJSnumbers_new RENAME TO YJSnumbers")

dbExecute(conn, "CREATE INDEX idx_YJSnumbers_ID_STRAIN ON YJSnumbers (ID_STRAIN)")

dbExecute(conn, "COMMIT")
dbExecute(conn, "PRAGMA foreign_keys = ON")

message("Migration complete. Row count: ",
        dbGetQuery(conn, "SELECT COUNT(*) AS n FROM YJSnumbers")$n)
