-- Revert opc_customers

BEGIN;

DROP TABLE IF EXISTS opc_customers;

COMMIT;
