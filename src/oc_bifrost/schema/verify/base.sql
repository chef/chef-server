-- Verify base

BEGIN;

-- Verify all the tables (and expected columns) are present.
SELECT id, authz_id FROM auth_container WHERE FALSE;
SELECT id, authz_id FROM auth_actor WHERE FALSE;
SELECT id, authz_id FROM auth_group WHERE FALSE;
SELECT id, authz_id FROM auth_object WHERE FALSE;

SELECT target, authorizee, permission FROM object_acl_group WHERE FALSE;
SELECT target, authorizee, permission FROM object_acl_actor WHERE FALSE;
SELECT target, authorizee, permission FROM actor_acl_group WHERE FALSE;
SELECT target, authorizee, permission FROM actor_acl_actor WHERE FALSE;
SELECT target, authorizee, permission FROM group_acl_actor WHERE FALSE;
SELECT target, authorizee, permission FROM group_acl_group WHERE FALSE;
SELECT target, authorizee, permission FROM container_acl_actor WHERE FALSE;
SELECT target, authorizee, permission FROM container_acl_group WHERE FALSE;

SELECT parent, child FROM group_group_relations WHERE FALSE;
SELECT parent, child FROM group_actor_relations WHERE FALSE;

-- Verify that the types exist; use division by zero as a way to
-- trigger an error if they're not present
SELECT 1/COUNT(*) FROM pg_type WHERE typname = 'auth_permission';
SELECT 1/COUNT(*) FROM pg_type WHERE typname = 'auth_any_permission';
SELECT 1/COUNT(*) FROM pg_type WHERE typname = 'auth_type';

-- TODO: Assert the desired indexes are present

ROLLBACK;
