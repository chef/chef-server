-- Revert migrate_org_migration_status_to_migratation_type

BEGIN;

DELETE FROM org_migration_state WHERE id = (SELECT id FROM org_migration_state WHERE org_id = (SELECT org_id FROM org_migration_state WHERE migration_type = 'purge_migration') AND migration_type = 'phase_1_migration');
UPDATE org_migration_state SET state = 'purge_successful' WHERE migration_type = 'purge_migration';

COMMIT;
