-- Verify id_resolution_functions

BEGIN;

SELECT pg_catalog.has_function_privilege('authz_id_for_type(character, auth_type)', 'execute');
SELECT pg_catalog.has_function_privilege('container_id(character)', 'execute');
SELECT pg_catalog.has_function_privilege('object_id(character)', 'execute');
SELECT pg_catalog.has_function_privilege('group_id(character)', 'execute');
SELECT pg_catalog.has_function_privilege('actor_id(character)', 'execute');

ROLLBACK;
