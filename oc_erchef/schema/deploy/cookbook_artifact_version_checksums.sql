-- Deploy cookbook_artifact_version_checksums
-- requires: cookbook_artifact_versions
-- requires: checksums

BEGIN;

CREATE TABLE IF NOT EXISTS cookbook_artifact_version_checksums(
  cookbook_artifact_version_id BIGINT NOT NULL REFERENCES cookbook_artifact_versions(id),
  org_id CHAR(32) NOT NULL,
  checksum CHAR(32) NOT NULL,
  FOREIGN KEY (org_id, checksum)
    REFERENCES checksums(org_id, checksum)
    ON DELETE RESTRICT
    ON UPDATE CASCADE
);

CREATE INDEX cookbook_artifact_version_checksums_by_id
  ON cookbook_artifact_version_checksums(cookbook_artifact_version_id);

CREATE INDEX cookbook_artifact_version_checksums_by_org_id_checksum
  ON cookbook_artifact_version_checksums(org_id, checksum);

COMMIT;
