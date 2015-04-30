-- Verify node_policy

BEGIN;

SELECT node_id, name, policy_group
FROM node_policy
WHERE FALSE;

ROLLBACK;
