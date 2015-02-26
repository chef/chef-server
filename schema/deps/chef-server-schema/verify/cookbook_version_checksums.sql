-- Verify cookbook_version_checksums

BEGIN;

SELECT cookbook_version_id, org_id, checksum
FROM cookbook_version_checksums
WHERE FALSE;

ROLLBACK;
