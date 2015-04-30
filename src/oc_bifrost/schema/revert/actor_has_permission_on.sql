-- Revert actor_has_permission_on

BEGIN;

DROP FUNCTION IF EXISTS actor_has_permission_on(character, character, auth_type, auth_any_permission);

COMMIT;
