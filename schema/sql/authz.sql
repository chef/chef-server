CREATE TYPE auth_permission AS ENUM ('update', 'read', 'grant', 'delete', 'create');

-- No unique constraint on names now because we don't have
-- organizations in Authz to do the proper scoping. :(
CREATE TABLE container(
    id   CHAR(32) PRIMARY KEY,
    name TEXT     NOT NULL
);

CREATE TABLE auth_actor(
    id CHAR(32) PRIMARY KEY
);

-- Can't scope Groups to organizations yet, because we have no
-- organizations!
CREATE TABLE auth_group(
       id CHAR(32) PRIMARY KEY
);

CREATE TABLE auth_object(
   id CHAR(32) PRIMARY KEY
);

-- ACL and membership graph information are kept in separate (but
-- similar) tables for actors and groups.  This makes for a simpler
-- schema, and allows for better use of foreign key constraints.
-- Recursive queries make relevant graph traversals straightforward.

-- Removing an Object removes all ACL and graph information
-- automatically via foreign key cascades
--
-- Deleting a user (or group) removes all related ACL and graph information
-- automatically via foreign key cascades

-- We can use the plv8 language to write JavaScript stored procedures
-- (for JSON manipulation).  We could create a stored proc that takes
-- ACL JSON and creates rows for them in the database. Similarly, we
-- could create a stored proc that generates the JSON for an entire
-- ACL entry

-- A record in an acl_* table indicates the permission is granted;
-- lack of a record indicates the permission is denied.
CREATE TABLE object_acl_group(
       target CHAR(32) NOT NULL REFERENCES auth_object(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee CHAR(32) NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

CREATE TABLE object_acl_actor(
       target CHAR(32) NOT NULL REFERENCES auth_object(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee CHAR(32) NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

CREATE TABLE actor_acl_group(
       target CHAR(32) NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee CHAR(32) NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

CREATE TABLE actor_acl_actor(
       target CHAR(32) NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee CHAR(32) NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

CREATE TABLE group_acl_group(
       target CHAR(32) NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee CHAR(32) NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

CREATE TABLE group_acl_actor(
       target CHAR(32) NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee CHAR(32) NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

CREATE TABLE container_acl_group(
       target CHAR(32) NOT NULL REFERENCES container(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee CHAR(32) NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

CREATE TABLE container_acl_actor(
       target CHAR(32) NOT NULL REFERENCES container(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee CHAR(32) NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);

-- Manage the group hierarchies

CREATE TABLE group_group_relations(
       parent CHAR(32) REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       child CHAR(32) REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       PRIMARY KEY (parent, child)
);

CREATE TABLE group_actor_relations(
       parent CHAR(32) REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       child CHAR(32) REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       PRIMARY KEY (parent, child)
);


-- Select all the actors in a group, recursively
CREATE FUNCTION actors_in_group(auth_group.id%TYPE)
RETURNS TABLE(actor auth_actor.id%TYPE)
LANGUAGE SQL
AS $$
    WITH RECURSIVE actors(g,a) AS (
    SELECT parent, child
    FROM group_actor_relations
    WHERE parent = $1
    AND child IS NOT NULL

    UNION

    SELECT gar.parent, gar.child
    FROM group_actor_relations AS gar
    JOIN group_group_relations AS ggr
    ON gar.parent = ggr.child
    JOIN actors ON actors.g = ggr.parent)
  SELECT DISTINCT(a) from actors;
$$;

-- Select all the groups an actor is a member of, recursively
CREATE FUNCTION groups_for_actor(auth_actor.id%TYPE)
RETURNS TABLE(auth_group auth_group.id%TYPE)
LANGUAGE SQL
AS $$
       WITH RECURSIVE groups(g,a) AS (
       SELECT parent, child
       FROM group_actor_relations
       WHERE child = $1
       AND child IS NOT NULL

       UNION

       SELECT ggr.parent, ggr.child
       FROM group_group_relations AS ggr
       JOIN group_actor_relations AS gar
       ON ggr.child = gar.parent
       JOIN groups ON groups.a = gar.child)
  SELECT DISTINCT(g) from groups;
$$;

-- Need functions for permission on container, actor, and group.
-- Don't think that we need similar functions for groups having a
-- permission, since I don't think that's ever queried for directly
-- from outside.
CREATE FUNCTION actor_has_permission_on_object(
       query_actor auth_actor.id%TYPE,
       query_object auth_object.id%TYPE,
       perm auth_permission)
RETURNS BOOLEAN
LANGUAGE plpgsql
AS $$
BEGIN
        -- Check to see if the actor has the permission directly
        PERFORM a.*
        FROM acl_actor AS a
        WHERE a.auth_object = query_object
        AND a.authorizee = query_actor
        AND a.permission = perm;

       -- If that returned anything, we're done
       IF FOUND THEN
          RETURN TRUE;
       END IF;

       -- The permission wasn't granted directly to the actor, we need
       -- to check the groups the actor is in
       PERFORM a.*
       FROM acl_group AS a
       JOIN groups_for_actor(query_actor)
          AS gs(id) ON gs.id = a.authorizee
       WHERE a.auth_object = query_object
       AND a.permission = perm;

       -- If anything was found, we're done
       IF FOUND THEN
          RETURN TRUE;
       END IF;

       -- The actor doesn't have the permission
       RETURN FALSE;
END;
$$;
