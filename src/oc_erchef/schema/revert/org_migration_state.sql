-- Revert org_migration_state

BEGIN;

DROP INDEX IF EXISTS org_migration_state_name_idx;
DROP TABLE IF EXISTS org_migration_state;
DROP TYPE IF EXISTS org_state;

COMMIT;
