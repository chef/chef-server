-- Deploy sandboxed_checksums

BEGIN;

CREATE TABLE IF NOT EXISTS sandboxed_checksums(
  org_id CHAR(32) NOT NULL,
  sandbox_id CHAR(32) NOT NULL,
  checksum CHAR(32) NOT NULL,
  PRIMARY KEY(sandbox_id, org_id, checksum),
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

COMMIT;
