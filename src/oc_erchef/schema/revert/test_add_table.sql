-- Revert test_add_table

BEGIN;

DROP TABLE IF EXISTS test_add_table;

COMMIT;
