-- Revert add_migration_type

BEGIN;

ALTER TABLE org_migration_state DROP COLUMN migration_type;

COMMIT;
