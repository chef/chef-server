-- Revert cookbook_versions

BEGIN;

DROP TABLE IF EXISTS cookbook_versions;

COMMIT;
