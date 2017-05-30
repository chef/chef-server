-- Deploy enterprise_chef:users_email_functional_index to pg
-- requires: users

BEGIN;

  -- TODO 2017/05/30 sr: check with ops if this should include CONCURRENTLY
  -- https://www.postgresql.org/docs/9.2/static/sql-createindex.html#SQL-CREATEINDEX-CONCURRENTLY
  CREATE INDEX users_lower_email_idx ON users (lower(email));

COMMIT;
