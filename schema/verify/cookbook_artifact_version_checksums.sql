-- Verify cookbook_artifact_version_checksums

BEGIN;

SELECT cookbook_artifact_version_id, org_id, checksum
FROM cookbook_artifact_version_checksums
WHERE FALSE;

ROLLBACK;
