-- Verify actor_has_bulk_permission_on

BEGIN;

SELECT has_function_privilege(
  'actor_has_bulk_permission_on(character, character[], auth_type, auth_permission)',
  'execute');

ROLLBACK;
