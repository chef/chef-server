-- Verify actor_has_permission_on

BEGIN;

SELECT pg_catalog.has_function_privilege(
  'actor_has_permission_on(character, character, auth_type, auth_any_permission)',
  'execute');

ROLLBACK;
