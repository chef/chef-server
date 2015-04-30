-- Deploy opc_users_customer_id_index
-- requires: opc_users

BEGIN;

CREATE INDEX opc_users_customer_id_index on opc_users(customer_id); 

COMMIT;
