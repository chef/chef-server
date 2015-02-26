-- Revert node_policy

BEGIN;

DROP TABLE IF EXISTS node_policy;

COMMIT;
