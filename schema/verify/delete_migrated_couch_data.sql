-- Verify delete_migrated_couch_data

BEGIN;

SELECT pg_catalog.has_function_privilege(
  'delete_migrated_couch_data(character)',
  'execute');

ROLLBACK;
