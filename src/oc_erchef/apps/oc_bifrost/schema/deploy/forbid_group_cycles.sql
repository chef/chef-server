-- Deploy forbid_group_cycles
-- requires: base

BEGIN;

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

COMMIT;
