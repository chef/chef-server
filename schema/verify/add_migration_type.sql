-- Verify add_migration_type

BEGIN;

SELECT migration_type FROM org_migration_state WHERE FALSE;

ROLLBACK;
