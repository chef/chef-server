-- Revert multiple_keys_migration

BEGIN;

DROP FUNCTION IF EXISTS migrate_keys();

COMMIT;
