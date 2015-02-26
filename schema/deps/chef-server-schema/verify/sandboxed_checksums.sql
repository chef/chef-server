-- Verify sandboxed_checksums

BEGIN;

SELECT org_id, sandbox_id, checksum, created_at
FROM sandboxed_checksums
WHERE FALSE;

ROLLBACK;
