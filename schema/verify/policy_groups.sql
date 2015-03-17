-- Verify policy_groups

BEGIN;

SELECT id, name, org_id, name, authz_id, last_updated_by, serialized_object
FROM policy_groups
WHERE FALSE;

ROLLBACK;
