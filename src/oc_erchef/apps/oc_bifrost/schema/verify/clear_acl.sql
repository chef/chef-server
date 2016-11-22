-- Verify clear_acl

BEGIN;

SELECT pg_catalog.has_function_privilege(
  'clear_acl(auth_type, character, auth_permission)',
  'execute');

ROLLBACK;
