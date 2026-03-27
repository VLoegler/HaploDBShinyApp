-- HaploDB SQLite Schema Reference
-- This file documents the full database structure across the three SQLite files.
-- It is not executed by the application — tables are created programmatically
-- by utils_db.R on first launch.

-- ============================================================================
-- haplodb.sqlite — Main database (data/haploDB/haplodb.sqlite)
-- ============================================================================

-- Core tables

CREATE TABLE IF NOT EXISTS Strains (
  STRAIN varchar(50) NOT NULL PRIMARY KEY,
  ISOLATION TEXT,
  ECO_ORIGIN TEXT,
  GEO_ORIGIN TEXT,
  CONTINENT TEXT,
  COUNTRY TEXT,
  CLADE TEXT,
  SRR_ID TEXT,
  SPECIES TEXT
);

CREATE TABLE IF NOT EXISTS YJSnumbers (
  YJS_NUMBER varchar(50) NOT NULL PRIMARY KEY,
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
  ID_STRAIN varchar(50) REFERENCES Strains(STRAIN),
  SAMPLE_TYPE TEXT,
  COLLECTION TEXT,
  CHECK (
    BOX_NUMBER IS NOT NULL
    OR
    (PLATE IS NOT NULL AND PLATE_ROW IS NOT NULL AND PLATE_COL IS NOT NULL)
  )
);

CREATE INDEX idx_YJSnumbers_ID_STRAIN ON YJSnumbers (ID_STRAIN);

CREATE TABLE IF NOT EXISTS AltNames (
  ID INTEGER PRIMARY KEY AUTOINCREMENT,
  STRAIN varchar(50) NOT NULL REFERENCES Strains(STRAIN),
  ALT_NAME varchar(50) NOT NULL
);

CREATE INDEX idx_AltNames_STRAIN ON AltNames (STRAIN);

CREATE TABLE IF NOT EXISTS AltYJS (
  OLD_YJS_NUMBER varchar(50) NOT NULL PRIMARY KEY,
  YJS_NUMBER varchar(50) NOT NULL REFERENCES YJSnumbers(YJS_NUMBER),
  DATE date
);

CREATE INDEX idx_AltYJS_YJS_NUMBER ON AltYJS (YJS_NUMBER);

-- Reference tables

CREATE TABLE IF NOT EXISTS Projects (
  ID_Project varchar(50) NOT NULL PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS Conditions (
  ID_CONDITION varchar(50) NOT NULL PRIMARY KEY,
  BASE_MEDIA TEXT NOT NULL,
  COMPOUND TEXT,
  CONCENTRATION TEXT,
  TEMPERATURE INTEGER,
  PH REAL
);

-- Data tables (FK -> YJSnumbers)

CREATE TABLE IF NOT EXISTS SeqData (
  ID_SeqData INTEGER PRIMARY KEY AUTOINCREMENT,
  PATH TEXT NOT NULL,
  METHOD TEXT,
  MOLECULE TEXT,
  FILETYPE TEXT,
  ORIGIN_LAB TEXT,
  YJS_NUMBER varchar(50) REFERENCES YJSnumbers(YJS_NUMBER),
  ID_Project varchar(50) REFERENCES Projects(ID_Project),
  COMMENT TEXT
);

CREATE INDEX idx_SeqData_YJS_NUMBER ON SeqData (YJS_NUMBER);
CREATE INDEX idx_SeqData_ID_Project ON SeqData (ID_Project);

CREATE TABLE IF NOT EXISTS Genotypes (
  ID_Genotype INTEGER PRIMARY KEY AUTOINCREMENT,
  PATH TEXT NOT NULL,
  METHOD TEXT,
  FORMAT TEXT,
  YJS_NUMBER varchar(50) NOT NULL REFERENCES YJSnumbers(YJS_NUMBER),
  ID_Project varchar(50) REFERENCES Projects(ID_Project)
);

CREATE INDEX idx_Genotypes_YJS_NUMBER ON Genotypes (YJS_NUMBER);
CREATE INDEX idx_Genotypes_ID_Project ON Genotypes (ID_Project);

CREATE TABLE IF NOT EXISTS Assemblies (
  ID_Assembly INTEGER PRIMARY KEY AUTOINCREMENT,
  PATH TEXT NOT NULL,
  METHOD TEXT,
  TYPE TEXT,
  YJS_NUMBER varchar(50) NOT NULL REFERENCES YJSnumbers(YJS_NUMBER),
  ID_Project varchar(50) REFERENCES Projects(ID_Project)
);

CREATE INDEX idx_Assemblies_YJS_NUMBER ON Assemblies (YJS_NUMBER);
CREATE INDEX idx_Assemblies_ID_Project ON Assemblies (ID_Project);

CREATE TABLE IF NOT EXISTS GrowthPhenotypes (
  ID INTEGER PRIMARY KEY AUTOINCREMENT,
  YJS_NUMBER varchar(50) NOT NULL REFERENCES YJSnumbers(YJS_NUMBER),
  DATE TEXT,
  USER TEXT,
  COND varchar(50) NOT NULL REFERENCES Conditions(ID_CONDITION),
  REF_COND varchar(50) NOT NULL REFERENCES Conditions(ID_CONDITION),
  SIZE REAL,
  SIZE_REF REAL,
  GROWTHRATIO REAL,
  TIMEPOINT TEXT,
  ID_Project varchar(50) REFERENCES Projects(ID_Project)
);

CREATE INDEX idx_GrowthPhenotypes_YJS_NUMBER ON GrowthPhenotypes (YJS_NUMBER);
CREATE INDEX idx_GrowthPhenotypes_COND ON GrowthPhenotypes (COND);
CREATE INDEX idx_GrowthPhenotypes_REF_COND ON GrowthPhenotypes (REF_COND);
CREATE INDEX idx_GrowthPhenotypes_ID_Project ON GrowthPhenotypes (ID_Project);

CREATE TABLE IF NOT EXISTS RNASeqPhenotypes (
  ID INTEGER PRIMARY KEY AUTOINCREMENT,
  YJS_NUMBER varchar(50) NOT NULL REFERENCES YJSnumbers(YJS_NUMBER),
  COND varchar(50) NOT NULL REFERENCES Conditions(ID_CONDITION),
  GENE TEXT NOT NULL,
  COUNT REAL,
  TPM REAL,
  ID_Project varchar(50) REFERENCES Projects(ID_Project)
);

CREATE INDEX idx_RNASeqPhenotypes_YJS_NUMBER ON RNASeqPhenotypes (YJS_NUMBER);
CREATE INDEX idx_RNASeqPhenotypes_COND ON RNASeqPhenotypes (COND);
CREATE INDEX idx_RNASeqPhenotypes_ID_Project ON RNASeqPhenotypes (ID_Project);

CREATE TABLE IF NOT EXISTS ProteomicsPhenotypes (
  ID INTEGER PRIMARY KEY AUTOINCREMENT,
  YJS_NUMBER varchar(50) NOT NULL REFERENCES YJSnumbers(YJS_NUMBER),
  COND varchar(50) NOT NULL REFERENCES Conditions(ID_CONDITION),
  PROTEIN TEXT NOT NULL,
  PROT_ABUNDANCE REAL,
  ID_Project varchar(50) REFERENCES Projects(ID_Project)
);

CREATE INDEX idx_ProteomicsPhenotypes_YJS_NUMBER ON ProteomicsPhenotypes (YJS_NUMBER);
CREATE INDEX idx_ProteomicsPhenotypes_COND ON ProteomicsPhenotypes (COND);
CREATE INDEX idx_ProteomicsPhenotypes_ID_Project ON ProteomicsPhenotypes (ID_Project);

-- ============================================================================
-- users.sqlite — User accounts (data/users.sqlite)
-- ============================================================================

CREATE TABLE IF NOT EXISTS users (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT UNIQUE NOT NULL,
  password_hash TEXT NOT NULL,
  role TEXT NOT NULL DEFAULT 'basic',    -- 'admin' or 'basic'
  created_at TEXT DEFAULT (datetime('now'))
);

-- ============================================================================
-- pending.sqlite — Pending submissions & app state (data/pending.sqlite)
-- ============================================================================

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
);

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
);

CREATE TABLE IF NOT EXISTS pending_altnames (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  STRAIN TEXT NOT NULL,
  ALT_NAME TEXT NOT NULL,
  submitted_by TEXT NOT NULL,
  submitted_at TEXT NOT NULL DEFAULT (datetime('now'))
);

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
);

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
);

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
);

CREATE TABLE IF NOT EXISTS pending_proteomics (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  YJS_NUMBER TEXT NOT NULL,
  COND TEXT NOT NULL,
  PROTEIN TEXT NOT NULL,
  PROT_ABUNDANCE REAL,
  ID_Project TEXT,
  submitted_by TEXT NOT NULL,
  submitted_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS custom_options (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  field_name TEXT NOT NULL,
  value TEXT NOT NULL,
  UNIQUE(field_name, value)
);

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
);
