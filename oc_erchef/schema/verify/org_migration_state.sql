-- Verify org_migration_state

BEGIN;

-- Verify that the type exists; use division by zero as a way to
-- trigger an error if it's not present
SELECT 1/COUNT(*) FROM pg_type WHERE typname = 'org_state';

SELECT id, org_name, org_id, state, fail_location,
       migration_start, migration_end
FROM org_migration_state
WHERE FALSE;

ROLLBACK;
