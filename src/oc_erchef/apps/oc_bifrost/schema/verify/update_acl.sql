-- Verify update_acl

BEGIN;

SELECT pg_catalog.has_function_privilege(
  'update_acl(auth_type, character, auth_permission, character[], character[])',
  'execute');

ROLLBACK;
