-- Verify policies

BEGIN;

SELECT id, name, policy_group, authz_id, org_id, last_updated_by,
       serialized_object
FROM policies
WHERE FALSE;


ROLLBACK;
