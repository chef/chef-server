-- Verify checksums

BEGIN;

SELECT org_id, checksum
FROM checksums
WHERE FALSE;

ROLLBACK;
