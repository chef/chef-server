-- Revert id_resolution_functions

BEGIN;

DROP FUNCTION IF EXISTS authz_id_for_type(character, auth_type);
DROP FUNCTION IF EXISTS container_id(character);
DROP FUNCTION IF EXISTS object_id(character);
DROP FUNCTION IF EXISTS group_id(character);
DROP FUNCTION IF EXISTS actor_id(character);

COMMIT;
