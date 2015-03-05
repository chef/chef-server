-- Verify opc_users

BEGIN;

SELECT user_id, customer_id
FROM opc_users
WHERE FALSE;

ROLLBACK;
