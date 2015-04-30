-- Verify orgs

BEGIN;

SELECT id, authz_id, name, full_name, 
       assigned_at, last_updated_by, created_at, updated_at
FROM orgs
WHERE FALSE;

ROLLBACK;
