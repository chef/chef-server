-- Verify org_user_associations

BEGIN;

SELECT org_id, user_id, 
       last_updated_by, created_at, updated_at
FROM org_user_associations
WHERE FALSE;

ROLLBACK;
