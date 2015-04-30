-- Verify osc_password_hash_type_update

BEGIN;

-- Make sure out enum exists
SELECT 1/COUNT(*) FROM pg_type WHERE typname = 'password_hash_type';

-- But we named it the same as the old enum, so lets make sure it has a new
-- value in it
-- Problem is, it is not easy to view the enum diffinition from a sql query
-- But it is possible, so we borrowed the query from here
-- https://stackoverflow.com/questions/10923213/postgres-enum-data-type-or-check-constraint
-- and then modified it to fit a verify check, so if the new value is not present
-- this will throw an exception of divide by zero and fail the verification

SELECT 1/COUNT(*)
FROM (
  SELECT n.nspname,
         t.typname,
         e.enumlabel
  FROM pg_type t JOIN
       pg_enum e ON t.oid = e.enumtypid JOIN
       pg_catalog.pg_namespace n on n.oid = t.typnamespace
  WHERE t.typname = 'password_hash_type'
  ) AS hashtype
WHERE  hashtype.enumlabel = 'erlang-bcrypt-0.5.0';

ROLLBACK;
