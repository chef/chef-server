-- Revert delete_migrated_couch_data

BEGIN;

DROP FUNCTION IF EXISTS delete_migrated_couch_data(checksums.org_id%TYPE);

COMMIT;
