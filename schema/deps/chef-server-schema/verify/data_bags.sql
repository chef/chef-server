-- Verify data_bags

BEGIN;

SELECT id, authz_id, org_id, name,
       last_updated_by, created_at,
       updated_at
FROM data_bags
WHERE FALSE;

ROLLBACK;
