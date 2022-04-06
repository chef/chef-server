-- Verify test_add_table

BEGIN;

SELECT id, authz_id, username, email
FROM test_add_table
WHERE FALSE;

ROLLBACK;
