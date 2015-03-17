-- Revert cookbook_artifact_versions

BEGIN;

DROP TABLE IF EXISTS cookbook_artifact_versions CASCADE;

COMMIT;
