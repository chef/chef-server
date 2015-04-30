-- Verify groups

BEGIN;

SELECT id, org_id, authz_id, name, 
       last_updated_by, created_at, updated_at
FROM groups
WHERE FALSE;

ROLLBACK;
