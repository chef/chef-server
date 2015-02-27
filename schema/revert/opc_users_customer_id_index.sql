-- Revert opc_users_comster_id_index

BEGIN;

DROP INDEX opc_users_customer_id_index; 

COMMIT;
