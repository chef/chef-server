-- Revert nodes

BEGIN;

DROP INDEX IF EXISTS nodes_org_id_environment_index;
DROP TABLE IF EXISTS nodes;

COMMIT;
