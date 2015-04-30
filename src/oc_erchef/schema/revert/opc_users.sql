-- Revert opc_users

BEGIN;

DROP TABLE IF EXISTS opc_users;

COMMIT;
