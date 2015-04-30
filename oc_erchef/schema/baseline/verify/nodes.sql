-- Verify nodes

BEGIN;

SELECT id, authz_id, org_id, name, environment,
       last_updated_by, created_at, updated_at, serialized_object
FROM nodes
WHERE FALSE;

ROLLBACK;
