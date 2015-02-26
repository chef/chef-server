-- Revert cookbook_artifacts

BEGIN;

DROP TABLE IF EXISTS cookbook_artifacts;

COMMIT;
