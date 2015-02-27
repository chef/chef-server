-- Deploy cookbook_version_checksums
-- requires: cookbook_versions
-- requires: checksums

BEGIN;

CREATE TABLE IF NOT EXISTS cookbook_version_checksums(
  cookbook_version_id CHAR(32) NOT NULL REFERENCES cookbook_versions(id),
  org_id CHAR(32) NOT NULL,
  checksum CHAR(32) NOT NULL,
  FOREIGN KEY (org_id, checksum)
    REFERENCES checksums(org_id, checksum)
    ON DELETE RESTRICT
    ON UPDATE CASCADE
);

CREATE INDEX cookbook_version_checksums_by_id
  ON cookbook_version_checksums(cookbook_version_id);

COMMIT;
