-- Deploy migrate_org_migration_status_to_migratation_type

BEGIN;

INSERT INTO org_migration_state ( org_name, org_id, state, fail_location, migration_start, migration_end, migration_type) SELECT org_name, org_id, 'completed', fail_location,migration_start, migration_end, 'purge_migration' from org_migration_state where state = 'purge_successful';

UPDATE org_migration_state SET state = 'completed' WHERE state = 'purge_successful';

COMMIT;
