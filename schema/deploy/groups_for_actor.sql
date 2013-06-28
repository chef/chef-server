-- Deploy groups_for_actor
-- requires: base

BEGIN;

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

COMMIT;
