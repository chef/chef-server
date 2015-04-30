-- Revert cookbook_artifact_version_checksums

BEGIN;

DROP TABLE IF EXISTS cookbook_artifact_version_checksums;

COMMIT;
