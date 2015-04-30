-- Revert cookbook_version_checksums

BEGIN;

DROP INDEX IF EXISTS cookbook_version_checksums_by_id;

DROP TABLE IF EXISTS cookbook_version_checksums;

COMMIT;
