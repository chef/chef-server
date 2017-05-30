-- Verify enterprise_chef:users_email_functional_index on pg

BEGIN;

  -- with input from https://www.postgresql.org/message-id/16343.1222138948%40sss.pgh.pa.us
  SELECT
    (1/COUNT(*)) AS result
  FROM pg_class t, pg_class i, pg_index d, pg_attribute a
  WHERE i.relkind = 'i'
    AND d.indexrelid = i.oid
    AND d.indisprimary = 'f'
    AND t.oid = d.indrelid
    AND a.attrelid = i.oid
    AND i.relnamespace IN (SELECT oid FROM pg_namespace WHERE nspname IN ('public'))
    AND t.relname = 'users'
    AND pg_get_indexdef(i.oid, a.attnum, false)='lower(email)';

ROLLBACK;
