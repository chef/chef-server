CREATE TYPE auth_permission AS ENUM ('update', 'read', 'grant', 'delete', 'create');

-- No unique constraint on names now because we don't have
-- organizations in Authz to do the proper scoping. :(
CREATE TABLE container(
    id bigserial PRIMARY KEY,
    authz_id  CHAR(32) NOT NULL UNIQUE,
    name TEXT     NOT NULL
);

CREATE TABLE auth_actor(
    id bigserial PRIMARY KEY,
    authz_id  CHAR(32) NOT NULL UNIQUE
);

-- Can't scope Groups to organizations yet, because we have no
-- organizations!
CREATE TABLE auth_group(
    id bigserial PRIMARY KEY,
    authz_id  CHAR(32) NOT NULL UNIQUE
);

CREATE TABLE auth_object(
    id bigserial PRIMARY KEY,
    authz_id  CHAR(32) NOT NULL UNIQUE
);

-- ACL and membership graph information are kept in separate (but
-- similar) tables for actors and groups.  This makes for a simpler
-- schema, and allows for better use of foreign key constraints.
-- Recursive queries make relevant graph traversals straightforward.
--
-- By keeping permission tables separate this way, we gain some "type
-- safety" with only foreign key constraints.  With this schema it
-- would be impossible for, say, a container to have permissions on an
-- actor.  However, putting everything into one table would make this
-- possible, absent some logic in triggers.
--
-- It also allows for potentially quicker access, since we can bypass
-- information that is irrelevant to our queries by using the
-- appropriate tables (e.g., if we are interested in an actor's
-- permissions on an object, we do not need to scan a table that also
-- has information on that actor's permissions on other actors,
-- groups, or containers).
--
-- We also don't have any NULLs in the tables, which eliminates an
-- entire class of problems related to SQL handling NULLs in
-- inconsistent ways.
--
-- A record in an acl table indicates the permission is granted;
-- lack of a record indicates the permission is denied.
CREATE TABLE object_acl_group(
       target bigint NOT NULL REFERENCES auth_object(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

CREATE TABLE object_acl_actor(
       target bigint NOT NULL REFERENCES auth_object(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

CREATE TABLE actor_acl_group(
       target bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

CREATE TABLE actor_acl_actor(
       target bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

CREATE TABLE group_acl_group(
       target bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

CREATE TABLE group_acl_actor(
       target bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

CREATE TABLE container_acl_group(
       target bigint NOT NULL REFERENCES container(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

CREATE TABLE container_acl_actor(
       target bigint NOT NULL REFERENCES container(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

-- Manage the group hierarchies

CREATE TABLE group_group_relations(
       parent bigint REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       child bigint REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       PRIMARY KEY (parent, child)
);

CREATE TABLE group_actor_relations(
       parent bigint REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       child bigint REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       PRIMARY KEY (parent, child)
);


-- These are to help adding data in terms of Authz IDs
-- TODO: Consider views with rewrite rules instead?
CREATE FUNCTION actor_id(auth_actor.authz_id%TYPE)
RETURNS auth_actor.id%TYPE
LANGUAGE SQL STABLE STRICT
AS $$
   SELECT id FROM auth_actor WHERE authz_id = $1;
$$;

CREATE FUNCTION group_id(auth_group.authz_id%TYPE)
RETURNS auth_group.id%TYPE
LANGUAGE SQL STABLE STRICT
AS $$
   SELECT id FROM auth_group WHERE authz_id = $1;
$$;

CREATE FUNCTION object_id(auth_object.authz_id%TYPE)
RETURNS auth_object.id%TYPE
LANGUAGE SQL STABLE STRICT
AS $$
   SELECT id FROM auth_object WHERE authz_id = $1;
$$;

CREATE FUNCTION container_id(container.authz_id%TYPE)
RETURNS container.id%TYPE
LANGUAGE SQL STABLE STRICT
AS $$
   SELECT id FROM container WHERE authz_id = $1;
$$;


-- Select all the groups an actor is a member of, recursively
--
-- NOTE: This function takes an internal database ID as an argument,
-- and returns a table of the same.  It is not intended to be called
-- directly from the outside.
CREATE OR REPLACE FUNCTION groups_for_actor(auth_actor.id%TYPE)
RETURNS TABLE(auth_group auth_group.id%TYPE)
LANGUAGE SQL STABLE STRICT
AS $$
WITH RECURSIVE
    groups(id) AS (
        -- direct group membership
        SELECT parent
        FROM group_actor_relations
        WHERE child = $1

        UNION

        -- indirect group membership; find parent groups of all groups
        -- the actor is a direct member of, recursively
        SELECT ggr.parent
        FROM group_group_relations AS ggr
        JOIN groups ON groups.id = ggr.child
    )
SELECT id from groups;
$$;

-- Need functions for permission on container, actor, and group.
-- Don't think that we need similar functions for groups having a
-- permission, since I don't think that's ever queried for directly
-- from outside.
CREATE FUNCTION actor_has_permission_on_object(
       query_actor auth_actor.authz_id%TYPE,
       query_object auth_object.authz_id%TYPE,
       perm auth_permission)
RETURNS BOOLEAN
LANGUAGE plpgsql
STABLE -- <- this function doesn't alter the database; just queries it
STRICT -- <- returns NULL immediately if any arguments are NULL
AS $$
DECLARE
  -- Convert Authz IDs into internal database IDs
  --
  -- If the Authz IDs don't refer to an existing item, an error will
  -- be thrown.
  actor_id  auth_actor.id%TYPE  NOT NULL := actor_id(query_actor);
  object_id auth_object.id%TYPE NOT NULL := object_id(query_object);
BEGIN

        -- Check to see if the actor has the permission directly
        PERFORM a.*
        FROM object_acl_actor AS a
        WHERE a.target = object_id
        AND a.authorizee = actor_id
        AND a.permission = perm;

       -- If that returned anything, we're done
       IF FOUND THEN
          RETURN TRUE;
       END IF;

       -- The permission wasn't granted directly to the actor, we need
       -- to check the groups the actor is in
       PERFORM a.*
       FROM object_acl_group AS a
       JOIN groups_for_actor(actor_id)
          AS gs(id) ON gs.id = a.authorizee
       WHERE a.target = object_id
       AND a.permission = perm;

       -- If anything was found, we're done
       IF FOUND THEN
          RETURN TRUE;
       END IF;

       -- The actor doesn't have the permission
       RETURN FALSE;
END;
$$;


--------------------------------------------------------------------------------
-- Debug Schema
--
-- Helpers for inspecting / debugging a live system... an on-call
-- dev's best friend.
--------------------------------------------------------------------------------

CREATE SCHEMA debug;

CREATE VIEW debug.object_acl AS
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
