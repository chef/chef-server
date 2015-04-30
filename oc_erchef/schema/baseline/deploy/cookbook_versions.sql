-- Deploy cookbook_versions
-- requires: cookbooks

BEGIN;

CREATE TABLE IF NOT EXISTS cookbook_versions(
  id CHAR(32) PRIMARY KEY,
  major BIGINT NOT NULL,
  minor BIGINT NOT NULL,
  patch BIGINT NOT NULL,
  frozen BOOLEAN NOT NULL, --DEFAULT FALSE
  meta_attributes bytea NOT NULL,
  meta_deps TEXT NOT NULL,
  meta_long_desc bytea NOT NULL,
  metadata bytea NOT NULL,
  serialized_object bytea NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  last_updated_by CHAR(32) NOT NULL,
  cookbook_id INTEGER NOT NULL REFERENCES cookbooks(id) ON DELETE RESTRICT,
  UNIQUE(cookbook_id, major, minor, patch)
);

ALTER TABLE cookbook_versions ALTER meta_attributes SET STORAGE EXTERNAL;
ALTER TABLE cookbook_versions ALTER meta_long_desc SET STORAGE EXTERNAL;
ALTER TABLE cookbook_versions ALTER metadata SET STORAGE EXTERNAL;
ALTER TABLE cookbook_versions ALTER serialized_object SET STORAGE EXTERNAL;

COMMIT;
