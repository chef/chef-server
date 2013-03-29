CREATE TYPE auth_permission AS ENUM ('update', 'read', 'grant', 'delete', 'create');
-- Used only for permission function (be nice if we could union this with 'any' and
-- the above, but didn't see a way to do that in the PG docs):
CREATE TYPE auth_any_permission AS ENUM ('update', 'read', 'grant', 'delete',
                                         'create', 'any');
CREATE TYPE auth_type AS ENUM ('actor', 'group', 'object', 'container');

-- No unique constraint on names now because we don't have
-- organizations in Authz to do the proper scoping. :(
CREATE TABLE auth_container(
    id bigserial PRIMARY KEY,
    authz_id  CHAR(32) NOT NULL UNIQUE
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
CREATE INDEX object_acl_group_authorizee ON object_acl_group(authorizee);

CREATE TABLE object_acl_actor(
       target bigint NOT NULL REFERENCES auth_object(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX object_acl_actor_authorizee ON object_acl_actor(authorizee);

CREATE TABLE actor_acl_group(
       target bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX actor_acl_group_authorizee ON actor_acl_group(authorizee);

CREATE TABLE actor_acl_actor(
       target bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX actor_acl_actor_authorizee ON actor_acl_actor(authorizee);

CREATE TABLE group_acl_group(
       target bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX group_acl_group_authorizee ON group_acl_group(authorizee);

CREATE TABLE group_acl_actor(
       target bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX group_acl_actor_authorizee ON group_acl_actor(authorizee);

CREATE TABLE container_acl_group(
       target bigint NOT NULL REFERENCES auth_container(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX container_acl_group_authorizee ON container_acl_group(authorizee);

CREATE TABLE container_acl_actor(
       target bigint NOT NULL REFERENCES auth_container(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX container_acl_actor_authorizee ON container_acl_actor(authorizee);

-- Manage the group hierarchies

CREATE TABLE group_group_relations(
       parent bigint REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       child bigint REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       PRIMARY KEY (parent, child),
       -- We can effectively perform this check in the
       -- no_long_range_cycles trigger (see below), but there's no
       -- harm in explicitly coding this simple (and cheap) base-case
       -- check
       CONSTRAINT no_trivial_cycles CHECK(parent != child)
);
CREATE INDEX group_group_relations_child ON group_group_relations(child);

CREATE TABLE group_actor_relations(
       parent bigint REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       child bigint REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       PRIMARY KEY (parent, child)
);
CREATE INDEX group_actor_relations_child ON group_actor_relations(child);

CREATE OR REPLACE FUNCTION forbid_group_cycles()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
DECLARE
    cycle_root BOOLEAN DEFAULT FALSE;
BEGIN
    SELECT INTO cycle_root (
        WITH RECURSIVE
            parents(id) AS (
                -- parent(s) of the current child group
                SELECT parent
                FROM group_group_relations
                WHERE child = NEW.child

                UNION

                -- grandparents and other ancestors
                SELECT ggr.parent
                FROM group_group_relations AS ggr
                JOIN parents AS p ON ggr.child = p.id
            )
        SELECT TRUE
        FROM parents
        WHERE id = NEW.child
    );

    IF cycle_root THEN
      RAISE EXCEPTION USING
        errcode='OC001',
        message='This would create a group membership cycle, which is not allowed';
    END IF;

    RETURN NULL;
END;
$$;

COMMENT ON FUNCTION forbid_group_cycles() IS
$$We want to forbid the presence of group membership cycles.  That is,
paths such as:

  X -member-of-> Y -member-of-> Z -member-of-> X

should be disallowed.  While the recursive group membership queries
are written to effectively ignore such cycles, forbidding cycles
allows us to eliminate a class of permission anomalies.

For instance, using the above cycle, if group X was given READ
permission (but not, say, DELETE) on an object and group Z was given
DELETE on that same object, by virtue of the cyclical membership
relationship, group X would inherit the DELETE permission!  If X was
supposed to be the root of this group hierarchy, it clearly should not
be inheriting DELETE from Z, its "child".  Alternatively, if Z the
true root, it would be legitimate for X to inherit DELETE from Z, but
not for Z to inherit READ!

Since the cyclic nature of such relationships prevents us from saying
which interpretation is correct, we explicitly forbid such scenarios
in the first place.

Put another way, by allowing group cycles, we are in effect declaring
that all actors and groups that participate in a cycle will have the
__union of all permissions granted to any of the groups__ in the cycle
(permissions granted directly to actors managed separately, and thus
are not involved in the present discussion).  Needless to say, this is
not a good state of affairs.

To implement this, the trigger function operates on each row that is
inserted.  If insertion of the row creates a cycle containing 'child'
(that is, if it creates a path by which 'child' may be reached from
itself), an exception is raised.
$$;

CREATE CONSTRAINT TRIGGER no_long_range_cycles
    AFTER INSERT OR UPDATE
    ON group_group_relations
    FOR EACH ROW
    EXECUTE PROCEDURE forbid_group_cycles();

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

CREATE FUNCTION container_id(auth_container.authz_id%TYPE)
RETURNS auth_container.id%TYPE
LANGUAGE SQL STABLE STRICT
AS $$
   SELECT id FROM auth_container WHERE authz_id = $1;
$$;

CREATE FUNCTION authz_id_for_type(char(32), auth_type)
RETURNS bigint
LANGUAGE plpgsql STABLE STRICT
AS $$
DECLARE
    return_id bigint;
BEGIN
    CASE $2
      WHEN 'actor' THEN
        SELECT INTO return_id id FROM auth_actor WHERE authz_id = $1;
      WHEN 'group' THEN
        SELECT INTO return_id id FROM auth_group WHERE authz_id = $1;
      WHEN 'object' THEN
        SELECT INTO return_id id FROM auth_object WHERE authz_id = $1;
      WHEN 'container' THEN
        SELECT INTO return_id id FROM auth_container WHERE authz_id = $1;
    END CASE;
    RETURN return_id;
END
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

-- Generic check for actors having permission on authz entities of arbitrary
-- type. Don't think that we need similar functions for groups having a permission,
-- since I don't think that's ever queried for directly from outside.
-- auth_any_permission includes all the regular permissions plus 'any' for testing
-- if query_actor has any permission
CREATE FUNCTION actor_has_permission_on(
       query_actor auth_actor.authz_id%TYPE,
       query_target char(32),
       target_type auth_type,
       perm auth_any_permission)
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
  actor_id  auth_actor.id%TYPE NOT NULL := actor_id(query_actor);
  target_id bigint NOT NULL := authz_id_for_type(query_target, target_type);
  actor_table char(32) := quote_ident(target_type || '_acl_actor');
  group_table char(32) := quote_ident(target_type || '_acl_group');
  has_result bigint;  
BEGIN
        -- Check to see if the actor has the permission directly
        EXECUTE 'SELECT a.target FROM ' || actor_table || ' AS a
            WHERE a.target = $1
            AND a.authorizee = $2
            AND ($3 = ''any'' OR a.permission::text = $3::text)'
          USING target_id, actor_id, perm
          INTO has_result;

       -- If that returned anything, we're done
       IF has_result IS NOT NULL THEN
          RETURN TRUE;
       END IF;

       -- The permission wasn't granted directly to the actor, we need
       -- to check the groups the actor is in
       EXECUTE 'SELECT a.target FROM ' || group_table || ' AS a
           JOIN groups_for_actor($1)
             AS gs(id) ON gs.id = a.authorizee
           WHERE a.target = $2
           AND ($3 = ''any'' OR a.permission::text = $3::text)'
         USING actor_id, target_id, perm
         INTO has_result;

       -- If anything was found, we're done
       IF has_result IS NOT NULL THEN
          RETURN TRUE;
       END IF;

       -- The actor doesn't have the permission
       RETURN FALSE;
END;
$$;

-- This is for creating an authz_entity, and also adding permissions on the entity
-- to an actor (presumably the requesting actor who created it).  This function will
-- put a newly created actor into its own ACL if the entity_type is actor.  Passing
-- a NULL creator_id is possible (e.g., in the case where this is something being
-- created by the superuser).
CREATE FUNCTION create_and_add_permissions(
       entity_type auth_type,
       entity_id char(32),
       creator_id char(32))
RETURNS BOOLEAN -- <- just returns true, since actually all we're doing is inserting stuff
LANGUAGE plpgsql
AS $$
DECLARE
        new_id bigint;
        requestor_id auth_actor.id%TYPE := actor_id(creator_id);
        entity_table char(32) := quote_ident('auth_' || entity_type);
        acl_table char(32) := quote_ident(entity_type || '_acl_actor');
BEGIN
        -- Create entity
        EXECUTE 'INSERT INTO ' || entity_table || '(authz_id)
            VALUES ($1) RETURNING id' USING entity_id
            INTO STRICT new_id;

        -- Add ACL on entity for requesting actor
        IF requestor_id IS NOT NULL THEN
          EXECUTE 'INSERT INTO ' || acl_table || '(target, authorizee, permission)
              VALUES ($1, $2, ''create''),
                     ($1, $2, ''read''),
                     ($1, $2, ''update''),
                     ($1, $2, ''delete''),
                     ($1, $2, ''grant'')' USING new_id, requestor_id;
        END IF;

        -- If entity is actor, give itself permissions
        IF entity_type = 'actor' THEN
          EXECUTE 'INSERT INTO ' || acl_table || '(target, authorizee, permission)
              VALUES ($1, $1, ''create''),
                     ($1, $1, ''read''),
                     ($1, $1, ''update''),
                     ($1, $1, ''delete''),
                     ($1, $1, ''grant'')' USING new_id;
        END IF;

        -- Need to return something, I guess?
        RETURN TRUE;
END;
$$;

-- Function for clearing ACLs for arbitrary entities
CREATE FUNCTION clear_acl(
       entity_type auth_type,
       entity_id char(32),
       perm auth_permission)
RETURNS BOOLEAN -- <- just returns true, since we're just updating DB tables
LANGUAGE plpgsql
AS $$
DECLARE
        actor_table char(32) := quote_ident(entity_type || '_acl_actor');
        group_table char(32) := quote_ident(entity_type || '_acl_group');
        target_id bigint NOT NULL := authz_id_for_type(entity_id, entity_type);
BEGIN
        EXECUTE 'DELETE FROM ' || actor_table || '
            WHERE target = $1 AND permission = $2' USING target_id, perm;
        EXECUTE 'DELETE FROM ' || group_table || '
            WHERE target = $1 AND permission = $2' USING target_id, perm;

        RETURN TRUE;
END;
$$;

-- Function for updating ACLs with arbitrary sets of groups and actors
CREATE FUNCTION update_acl(
       entity_type auth_type,
       entity_id char(32),
       perm auth_permission,
       actors char(32)[],
       groups char(32)[])
RETURNS BOOLEAN -- <- just returns true, since we're just updating DB tables
LANGUAGE plpgsql
AS $$
DECLARE
        actor_table char(32) := quote_ident(entity_type || '_acl_actor');
        group_table char(32) := quote_ident(entity_type || '_acl_group');
        target_id bigint NOT NULL := authz_id_for_type(entity_id, entity_type);
        count integer;
BEGIN
        -- Clear out the old
        EXECUTE 'DELETE FROM ' || actor_table || '
            WHERE target = $1 AND permission = $2' USING target_id, perm;
        EXECUTE 'DELETE FROM ' || group_table || '
            WHERE target = $1 AND permission = $2' USING target_id, perm;

        -- Insert the new
        count := 0;
        LOOP
          IF array_upper(actors, 1) IS NULL OR count > array_upper(actors, 1) THEN
            EXIT;
          END IF;
          EXECUTE 'INSERT INTO ' || actor_table || '(target, authorizee, permission)
            VALUES ($1, $2, $3)' USING target_id, actor_id(actors[count]), perm;
          count := count + 1;
        END LOOP;

        -- Insert the new
        count := 0;
        LOOP
          IF array_upper(groups, 1) IS NULL OR count > array_upper(groups, 1) THEN
            EXIT;
          END IF;
          EXECUTE 'INSERT INTO ' || group_table || '(target, authorizee, permission)
            VALUES ($1, $2, $3)' USING target_id, group_id(groups[count]), perm;
          count := count + 1;
        END LOOP;

        RETURN TRUE;
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
