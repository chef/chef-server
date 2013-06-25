-- Verify create_and_add_permissions

BEGIN;

SELECT pg_catalog.has_function_privilege(
  'create_and_add_permissions(auth_type, character, character)',
  'execute');

ROLLBACK;
