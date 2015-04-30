-- Revert sandboxed_checksums

BEGIN;

DROP TABLE IF EXISTS sandboxed_checksums;

COMMIT;
