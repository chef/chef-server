-- Deploy test_add_table

BEGIN;

CREATE TABLE IF NOT EXISTS test_add_table(
  id CHAR(32) PRIMARY KEY,
  authz_id CHAR(32) NOT NULL UNIQUE,
  username TEXT NOT NULL UNIQUE,
  email TEXT UNIQUE
);

COMMIT;
