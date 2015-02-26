-- Verify org_user_invites

BEGIN;

SELECT id, org_id, user_id, 
       last_updated_by, created_at, updated_at
FROM org_user_invites
WHERE FALSE;


ROLLBACK;
