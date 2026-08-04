-- HaploDB PostgreSQL schema target
-- PostgreSQL-compatible DDL derived from the SQLite reference schema.
-- This file is intended for use with an empty PostgreSQL database.


-- Migration mapping
--
-- SQLite table        -> PostgreSQL table
-- Strains             -> strains
-- YJSnumbers          -> yjs_numbers
-- AltNames            -> alt_names
-- AltYJS              -> alt_yjs
-- SeqData             -> seq_data
-- GrowthPhenotypes    -> growth_phenotypes
--
-- Source of truth for users:
-- users.sqlite.users
--
-- Ignore legacy users table that may exist in haplodb.sqlite.


-- ============================================================================
-- Main HaploDB tables
-- ============================================================================

CREATE TABLE IF NOT EXISTS strains (
  strain VARCHAR(50) NOT NULL PRIMARY KEY,
  isolation TEXT,
  eco_origin TEXT,
  geo_origin TEXT,
  continent TEXT,
  country TEXT,
  clade TEXT,
  srr_id TEXT,
  species TEXT
);

CREATE TABLE IF NOT EXISTS yjs_numbers (
  yjs_number VARCHAR(50) NOT NULL PRIMARY KEY,
  sample_name TEXT,
  species TEXT,
  mating_type TEXT,
  ploidy TEXT,
  genotype TEXT,
  sporulation TEXT,
  external_origin TEXT,
  eco_origin TEXT,
  comments_origin TEXT,
  parental_origin TEXT,
  publication TEXT,
  strains_group TEXT,
  old_box INTEGER,
  box_number INTEGER,
  box_row INTEGER,
  box_col INTEGER,
  plate INTEGER,
  plate_row INTEGER,
  plate_col INTEGER,
  notes TEXT,
  stocked_by TEXT,
  comments TEXT,
  id_strain VARCHAR(50),
  sample_type TEXT,
  collection TEXT,
  CONSTRAINT fk_yjs_numbers_strain
    FOREIGN KEY (id_strain) REFERENCES strains (strain),
  CONSTRAINT chk_yjs_numbers_location
    CHECK (
      box_number IS NOT NULL
      OR
      (plate IS NOT NULL AND plate_row IS NOT NULL AND plate_col IS NOT NULL)
    )
);

CREATE TABLE IF NOT EXISTS alt_names (
  id BIGSERIAL PRIMARY KEY,
  strain VARCHAR(50) NOT NULL,
  alt_name VARCHAR(50) NOT NULL,
  CONSTRAINT fk_alt_names_strain
    FOREIGN KEY (strain) REFERENCES strains (strain)
);

CREATE TABLE IF NOT EXISTS alt_yjs (
  old_yjs_number VARCHAR(50) NOT NULL PRIMARY KEY,
  yjs_number VARCHAR(50) NOT NULL,
  date DATE,
  CONSTRAINT fk_alt_yjs_yjs_number
    FOREIGN KEY (yjs_number) REFERENCES yjs_numbers (yjs_number)
);

-- ============================================================================
-- Reference tables
-- ============================================================================

CREATE TABLE IF NOT EXISTS projects (
  id_project VARCHAR(50) NOT NULL PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS conditions (
  id_condition VARCHAR(50) NOT NULL PRIMARY KEY,
  base_media TEXT NOT NULL,
  compound TEXT,
  concentration TEXT,
  temperature INTEGER,
  ph DOUBLE PRECISION
);

-- ============================================================================
-- Data tables
-- ============================================================================

CREATE TABLE IF NOT EXISTS seq_data (
  id_seq_data BIGSERIAL PRIMARY KEY,
  path TEXT NOT NULL,
  method TEXT,
  molecule TEXT,
  filetype TEXT,
  origin_lab TEXT,
  yjs_number VARCHAR(50),
  id_project VARCHAR(50),
  comment TEXT,
  CONSTRAINT fk_seq_data_yjs_number
    FOREIGN KEY (yjs_number) REFERENCES yjs_numbers (yjs_number),
  CONSTRAINT fk_seq_data_project
    FOREIGN KEY (id_project) REFERENCES projects (id_project)
);

CREATE TABLE IF NOT EXISTS genotypes (
  id_genotype BIGSERIAL PRIMARY KEY,
  path TEXT NOT NULL,
  method TEXT,
  format TEXT,
  yjs_number VARCHAR(50) NOT NULL,
  id_project VARCHAR(50),
  CONSTRAINT fk_genotypes_yjs_number
    FOREIGN KEY (yjs_number) REFERENCES yjs_numbers (yjs_number),
  CONSTRAINT fk_genotypes_project
    FOREIGN KEY (id_project) REFERENCES projects (id_project)
);

CREATE TABLE IF NOT EXISTS assemblies (
  id_assembly BIGSERIAL PRIMARY KEY,
  path TEXT NOT NULL,
  method TEXT,
  type TEXT,
  yjs_number VARCHAR(50) NOT NULL,
  id_project VARCHAR(50),
  CONSTRAINT fk_assemblies_yjs_number
    FOREIGN KEY (yjs_number) REFERENCES yjs_numbers (yjs_number),
  CONSTRAINT fk_assemblies_project
    FOREIGN KEY (id_project) REFERENCES projects (id_project)
);

CREATE TABLE IF NOT EXISTS growth_phenotypes (
  id BIGSERIAL PRIMARY KEY,
  yjs_number VARCHAR(50) NOT NULL,
  date TEXT,
  user TEXT,
  cond VARCHAR(50) NOT NULL,
  ref_cond VARCHAR(50) NOT NULL,
  size DOUBLE PRECISION,
  size_ref DOUBLE PRECISION,
  growthratio DOUBLE PRECISION,
  timepoint TEXT,
  id_project VARCHAR(50),
  CONSTRAINT fk_growth_phenotypes_yjs_number
    FOREIGN KEY (yjs_number) REFERENCES yjs_numbers (yjs_number),
  CONSTRAINT fk_growth_phenotypes_cond
    FOREIGN KEY (cond) REFERENCES conditions (id_condition),
  CONSTRAINT fk_growth_phenotypes_ref_cond
    FOREIGN KEY (ref_cond) REFERENCES conditions (id_condition),
  CONSTRAINT fk_growth_phenotypes_project
    FOREIGN KEY (id_project) REFERENCES projects (id_project)
);

-- ============================================================================
-- User accounts
-- ============================================================================

CREATE TABLE IF NOT EXISTS users (
  id BIGSERIAL PRIMARY KEY,
  username TEXT UNIQUE NOT NULL,
  password_hash TEXT NOT NULL,
  role TEXT NOT NULL DEFAULT 'basic',
  CONSTRAINT chk_users_role
      CHECK (role IN ('admin', 'basic')),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- ============================================================================
-- Pending submissions and app state
-- ============================================================================

CREATE TABLE IF NOT EXISTS pending_yjs (
  id BIGSERIAL PRIMARY KEY,
  yjs_number TEXT NOT NULL,
  sample_name TEXT,
  species TEXT,
  mating_type TEXT,
  ploidy TEXT,
  genotype TEXT,
  sporulation TEXT,
  external_origin TEXT,
  eco_origin TEXT,
  comments_origin TEXT,
  parental_origin TEXT,
  publication TEXT,
  strains_group TEXT,
  old_box INTEGER,
  box_number INTEGER,
  box_row INTEGER,
  box_col INTEGER,
  plate INTEGER,
  plate_row INTEGER,
  plate_col INTEGER,
  notes TEXT,
  stocked_by TEXT,
  comments TEXT,
  id_strain TEXT,
  sample_type TEXT,
  collection TEXT,
  submitted_by TEXT NOT NULL,
  submitted_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS pending_strains (
  id BIGSERIAL PRIMARY KEY,
  strain TEXT NOT NULL,
  isolation TEXT,
  eco_origin TEXT,
  geo_origin TEXT,
  continent TEXT,
  country TEXT,
  clade TEXT,
  srr_id TEXT,
  species TEXT,
  original_name TEXT NOT NULL,
  submitted_by TEXT NOT NULL,
  submitted_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS pending_altnames (
  id BIGSERIAL PRIMARY KEY,
  strain TEXT NOT NULL,
  alt_name TEXT NOT NULL,
  submitted_by TEXT NOT NULL,
  submitted_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS pending_seqdata (
  id BIGSERIAL PRIMARY KEY,
  path TEXT NOT NULL,
  method TEXT,
  molecule TEXT,
  filetype TEXT,
  origin_lab TEXT,
  yjs_number TEXT,
  id_project TEXT,
  comment TEXT,
  submitted_by TEXT NOT NULL,
  submitted_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS pending_growth (
  id BIGSERIAL PRIMARY KEY,
  yjs_number TEXT NOT NULL,
  date TEXT,
  user TEXT,
  cond TEXT NOT NULL,
  ref_cond TEXT NOT NULL,
  size DOUBLE PRECISION,
  size_ref DOUBLE PRECISION,
  growthratio DOUBLE PRECISION,
  timepoint TEXT,
  id_project TEXT,
  submitted_by TEXT NOT NULL,
  submitted_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS custom_options (
  id BIGSERIAL PRIMARY KEY,
  field_name TEXT NOT NULL,
  value TEXT NOT NULL,
  UNIQUE (field_name, value)
);

CREATE TABLE IF NOT EXISTS notifications (
  id BIGSERIAL PRIMARY KEY,
  username TEXT NOT NULL,
  entry_type TEXT NOT NULL,
  entry_name TEXT,
  assigned_number TEXT,
  status TEXT NOT NULL,
  reviewer TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  is_read BOOLEAN NOT NULL DEFAULT FALSE,
  box TEXT,
  box_row TEXT,
  box_col TEXT,
  plate TEXT,
  plate_row TEXT,
  plate_col TEXT
);

-- ============================================================================
-- Indexes
-- ============================================================================

CREATE INDEX IF NOT EXISTS idx_yjs_numbers_id_strain ON yjs_numbers (id_strain);
CREATE INDEX IF NOT EXISTS idx_alt_names_strain ON alt_names (strain);
CREATE INDEX IF NOT EXISTS idx_alt_yjs_yjs_number ON alt_yjs (yjs_number);
CREATE INDEX IF NOT EXISTS idx_seq_data_yjs_number ON seq_data (yjs_number);
CREATE INDEX IF NOT EXISTS idx_seq_data_id_project ON seq_data (id_project);
CREATE INDEX IF NOT EXISTS idx_genotypes_yjs_number ON genotypes (yjs_number);
CREATE INDEX IF NOT EXISTS idx_genotypes_id_project ON genotypes (id_project);
CREATE INDEX IF NOT EXISTS idx_assemblies_yjs_number ON assemblies (yjs_number);
CREATE INDEX IF NOT EXISTS idx_assemblies_id_project ON assemblies (id_project);
CREATE INDEX IF NOT EXISTS idx_growth_phenotypes_yjs_number ON growth_phenotypes (yjs_number);
CREATE INDEX IF NOT EXISTS idx_growth_phenotypes_cond ON growth_phenotypes (cond);
CREATE INDEX IF NOT EXISTS idx_growth_phenotypes_ref_cond ON growth_phenotypes (ref_cond);
CREATE INDEX IF NOT EXISTS idx_growth_phenotypes_id_project ON growth_phenotypes (id_project);
CREATE INDEX IF NOT EXISTS idx_notifications_username ON notifications (username);
CREATE INDEX IF NOT EXISTS idx_pending_yjs_submitted_by ON pending_yjs (submitted_by);
CREATE INDEX IF NOT EXISTS idx_pending_strains_submitted_by ON pending_strains (submitted_by);
CREATE INDEX IF NOT EXISTS idx_pending_altnames_submitted_by ON pending_altnames (submitted_by);
CREATE INDEX IF NOT EXISTS idx_pending_seqdata_submitted_by ON pending_seqdata (submitted_by);
CREATE INDEX IF NOT EXISTS idx_pending_growth_submitted_by ON pending_growth (submitted_by);