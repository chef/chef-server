-- Verify roles

BEGIN;

SELECT id, authz_id, org_id, name,
       last_updated_by, created_at,
       updated_at, serialized_object
FROM roles
WHERE FALSE;

ROLLBACK;
