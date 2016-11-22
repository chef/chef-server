-- Deploy base

BEGIN;

CREATE TYPE auth_permission AS ENUM ('update', 'read', 'grant', 'delete', 'create');
-- Used only for permission function (be nice if we could union this with 'any' and
-- the above, but didn't see a way to do that in the PG docs):
CREATE TYPE auth_any_permission AS ENUM ('update', 'read', 'grant', 'delete',
                                         'create', 'any');
CREATE TYPE auth_type AS ENUM ('actor', 'group', 'object', 'container');

-- No unique constraint on names now because we don't have
-- organizations in Authz to do the proper scoping. :(
CREATE TABLE IF NOT EXISTS auth_container(
    id bigserial PRIMARY KEY,
    authz_id  CHAR(32) NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS auth_actor(
    id bigserial PRIMARY KEY,
    authz_id  CHAR(32) NOT NULL UNIQUE
);

-- Can't scope Groups to organizations yet, because we have no
-- organizations!
CREATE TABLE IF NOT EXISTS auth_group(
    id bigserial PRIMARY KEY,
    authz_id  CHAR(32) NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS auth_object(
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
CREATE TABLE IF NOT EXISTS object_acl_group(
       target bigint NOT NULL REFERENCES auth_object(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX object_acl_group_authorizee ON object_acl_group(authorizee);

CREATE TABLE IF NOT EXISTS object_acl_actor(
       target bigint NOT NULL REFERENCES auth_object(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX object_acl_actor_authorizee ON object_acl_actor(authorizee);

CREATE TABLE IF NOT EXISTS actor_acl_group(
       target bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX actor_acl_group_authorizee ON actor_acl_group(authorizee);

CREATE TABLE IF NOT EXISTS actor_acl_actor(
       target bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX actor_acl_actor_authorizee ON actor_acl_actor(authorizee);

CREATE TABLE IF NOT EXISTS group_acl_group(
       target bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX group_acl_group_authorizee ON group_acl_group(authorizee);

CREATE TABLE IF NOT EXISTS group_acl_actor(
       target bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX group_acl_actor_authorizee ON group_acl_actor(authorizee);

CREATE TABLE IF NOT EXISTS container_acl_group(
       target bigint NOT NULL REFERENCES auth_container(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX container_acl_group_authorizee ON container_acl_group(authorizee);

CREATE TABLE IF NOT EXISTS container_acl_actor(
       target bigint NOT NULL REFERENCES auth_container(id) ON UPDATE CASCADE ON DELETE CASCADE,
       authorizee bigint NOT NULL REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       permission auth_permission NOT NULL,
       PRIMARY KEY (target, authorizee, permission)
);
CREATE INDEX container_acl_actor_authorizee ON container_acl_actor(authorizee);

-- Manage the group hierarchies

CREATE TABLE IF NOT EXISTS group_group_relations(
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

CREATE TABLE IF NOT EXISTS group_actor_relations(
       parent bigint REFERENCES auth_group(id) ON UPDATE CASCADE ON DELETE CASCADE,
       child bigint REFERENCES auth_actor(id) ON UPDATE CASCADE ON DELETE CASCADE,
       PRIMARY KEY (parent, child)
);
CREATE INDEX group_actor_relations_child ON group_actor_relations(child);

COMMIT;
