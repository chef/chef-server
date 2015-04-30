-- Deploy cookbook_artifact_versions
-- requires: cookbook_artifacts

BEGIN;

CREATE TABLE IF NOT EXISTS cookbook_artifact_versions(
  id BIGSERIAL PRIMARY KEY,
  identifier VARCHAR(255) NOT NULL,
  metadata bytea NOT NULL,
  serialized_object bytea NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  created_by CHAR(32) NOT NULL,
  cookbook_artifact_id INTEGER NOT NULL REFERENCES cookbook_artifacts(id) ON DELETE RESTRICT,
  UNIQUE(cookbook_artifact_id, identifier)
);

ALTER TABLE cookbook_artifact_versions ALTER metadata SET STORAGE EXTERNAL;
ALTER TABLE cookbook_artifact_versions ALTER serialized_object SET STORAGE EXTERNAL;

COMMIT;
