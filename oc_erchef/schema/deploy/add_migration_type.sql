-- Deploy add_migration_type

BEGIN;

ALTER TABLE org_migration_state ADD COLUMN migration_type TEXT NOT NULL default 'phase_1_migration';

COMMIT;
