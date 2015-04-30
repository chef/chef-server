-- Revert base

BEGIN;

DROP TABLE IF EXISTS group_actor_relations;
DROP TABLE IF EXISTS group_group_relations;

DROP TABLE IF EXISTS container_acl_actor;
DROP TABLE IF EXISTS container_acl_group;
DROP TABLE IF EXISTS group_acl_actor;
DROP TABLE IF EXISTS group_acl_group;
DROP TABLE IF EXISTS actor_acl_actor;
DROP TABLE IF EXISTS actor_acl_group;
DROP TABLE IF EXISTS object_acl_actor;
DROP TABLE IF EXISTS object_acl_group;

DROP TABLE IF EXISTS auth_object;
DROP TABLE IF EXISTS auth_group;
DROP TABLE IF EXISTS auth_actor;
DROP TABLE IF EXISTS auth_container;

DROP TYPE IF EXISTS auth_type;
DROP TYPE IF EXISTS auth_any_permission;
DROP TYPE IF EXISTS auth_permission;

COMMIT;
