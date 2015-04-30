-- Deploy debug_object_acl_view
-- requires: debug_schema
-- requires: base

BEGIN;

CREATE OR REPLACE VIEW debug.object_acl AS
WITH RECURSIVE
    all_groups(target, authorizee, perm, direct) AS (
        -- direct groups
        SELECT target, authorizee, permission, TRUE
        FROM object_acl_group

        UNION

       -- indirect groups
        SELECT all_groups.target, rel.child, all_groups.perm, FALSE
        FROM group_group_relations AS rel
        JOIN all_groups
          ON all_groups.authorizee = rel.parent
    ),

    all_actors(target, authorizee, perm, direct) AS (
        -- direct actors
        SELECT target, authorizee, permission, TRUE
        FROM object_acl_actor

        UNION

        -- indirect actors
        SELECT gs.target, rel.child, gs.perm, FALSE
        FROM group_actor_relations AS rel
        JOIN all_groups AS gs
          ON rel.parent = gs.authorizee
    )
SELECT o.authz_id AS "object",
       a.authz_id AS "authorizee",
       'actor' AS "type",
        perm AS permission,
        direct AS directly_granted
FROM all_actors
JOIN auth_object AS o
  ON all_actors.target = o.id
JOIN auth_actor AS a
  ON all_actors.authorizee = a.id

UNION

SELECT o.authz_id AS "object",
       g.authz_id AS "authorizee",
       'group' AS "type",
       perm AS permission,
       direct AS directly_granted
FROM all_groups
JOIN auth_object AS o
  ON all_groups.target = o.id
JOIN auth_group AS g
  ON all_groups.authorizee = g.id
;

COMMENT ON VIEW debug.object_acl IS
    'Shows all directly and indirectly granted permissions for all actors and groups on objects';
COMMENT ON COLUMN debug.object_acl.object IS
    'The Authz ID of an object';
COMMENT ON COLUMN debug.object_acl.authorizee IS
    'The Authz ID of an actor or group with a permission on `object`';
COMMENT ON COLUMN debug.object_acl.type IS
    'Either "actor" or "group"; refers to `authorizee`';
COMMENT ON COLUMN debug.object_acl.permission IS
    'The permission `authorizee` has on `object`';
COMMENT ON COLUMN debug.object_acl.directly_granted IS
    'Is `permission` directly granted, or due to group membership?';

COMMIT;
