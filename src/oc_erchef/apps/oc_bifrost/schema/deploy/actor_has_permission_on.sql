-- Deploy actor_has_permission_on
-- requires: base
-- requires: id_resolution_functions

BEGIN;

-- Generic check for actors having permission on authz entities of arbitrary
-- type. Don't think that we need similar functions for groups having a permission,
-- since I don't think that's ever queried for directly from outside.
-- auth_any_permission includes all the regular permissions plus 'any' for testing
-- if query_actor has any permission
CREATE OR REPLACE FUNCTION actor_has_permission_on(
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

COMMIT;
