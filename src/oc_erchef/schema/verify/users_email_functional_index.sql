-- Verify enterprise_chef:users_email_functional_index on pg

BEGIN;

  -- NOTE 2017/06/01 sr: This snippet is how it would be tested.
  -- However, for the sake of hosted (with many users), we've added CONCURRENTLY
  -- to the CREATE INDEX call.
  -- Thus for very large tables, the index creating might not have finished when
  -- this verify script is executed.
  -- Since the query this index meant to support will be fast for deployments
  -- with few users with or without the index, and thus work irregardless of the
  -- success of the index creation, we don't verify if it was successful.
  -- This seems like the smoothest tradeoff for all concerned parties.

  -- with input from https://www.postgresql.org/message-id/16343.1222138948%40sss.pgh.pa.us
  -- SELECT
  --   (1/COUNT(*)) AS result
  -- FROM pg_class t, pg_class i, pg_index d, pg_attribute a
  -- WHERE i.relkind = 'i'
  --   AND d.indexrelid = i.oid
  --   AND d.indisprimary = 'f'
  --   AND t.oid = d.indrelid
  --   AND a.attrelid = i.oid
  --   AND i.relnamespace IN (SELECT oid FROM pg_namespace WHERE nspname IN ('public'))
  --   AND t.relname = 'users'
  --   AND pg_get_indexdef(i.oid, a.attnum, false)='lower(email)';

ROLLBACK;
