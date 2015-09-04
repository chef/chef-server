-- Verify node_policy_name_policy_group

BEGIN;

SELECT id, authz_id, org_id, name, environment, policy_group, policy_name,
last_updated_by, created_at, updated_at, serialized_object
FROM nodes
WHERE FALSE;

ROLLBACK;
