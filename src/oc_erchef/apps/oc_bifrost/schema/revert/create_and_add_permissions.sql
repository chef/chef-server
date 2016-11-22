-- Revert create_and_add_permissions

BEGIN;

DROP FUNCTION IF EXISTS create_and_add_permissions(auth_type, character, character);

COMMIT;
