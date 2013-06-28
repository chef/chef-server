-- Revert actor_has_bulk_permission_on

BEGIN;

DROP FUNCTION IF EXISTS actor_has_bulk_permission_on(
     character,
     char[],
     auth_type,
     auth_permission);

COMMIT;
