-- Revert node_policy_name_policy_group

BEGIN;

DROP INDEX IF EXISTS nodes_policy_group;
DROP INDEX IF EXISTS nodes_policy_name;

ALTER TABLE nodes DROP COLUMN policy_group;
ALTER TABLE nodes DROP COLUMN policy_name;

COMMIT;
