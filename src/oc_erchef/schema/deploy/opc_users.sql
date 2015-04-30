-- Deploy opc_users
-- requires: users
-- requires: opc_customers

BEGIN;

CREATE TABLE IF NOT EXISTS opc_users(
  user_id CHAR(32) NOT NULL REFERENCES users(id) ON DELETE CASCADE, -- not on update?
  customer_id INTEGER REFERENCES opc_customers(id) ON DELETE CASCADE, -- not on update?
  UNIQUE(user_id, customer_id)
  -- should have a PK instead of UNIQUE
);

COMMIT;
