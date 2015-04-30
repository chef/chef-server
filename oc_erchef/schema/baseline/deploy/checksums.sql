-- Deploy checksums

BEGIN;

CREATE TABLE IF NOT EXISTS checksums(
  org_id CHAR(32) NOT NULL,
  checksum CHAR(32) NOT NULL,
  PRIMARY KEY (org_id, checksum)
);

COMMIT;
