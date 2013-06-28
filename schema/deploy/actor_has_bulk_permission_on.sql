-- Deploy actor_has_bulk_permission_on

BEGIN;

-- This is a bulk permission check for actors having permission on a set of authz
-- entities of arbitrary (but the same) type.  We don't support "any" permission
-- here.  Although it wouldn't be particularly difficult to make that happen (we
-- have the types and such in place as you can see from actor_has_permission_on
-- above), we don't currently have a use case for it, either.
CREATE OR REPLACE FUNCTION actor_has_bulk_permission_on(
       query_actor auth_actor.authz_id%TYPE,
       query_targets char(32)[],
       target_type auth_type,
       perm auth_permission)
RETURNS setof char(32) -- <- this returns the IDs that are authorized
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
  actor_table char(32) := quote_ident(target_type || '_acl_actor');
  group_table char(32) := quote_ident(target_type || '_acl_group');
  authz_table char(32) := quote_ident('auth_' || target_type);
BEGIN
        -- Get everything in our supplied targets that query_actor has permission on;
        -- to keep things simple, we union the direct and indirect permission
        -- results.  It's easier than accumulating and merging results (possibly
        -- faster, too, at some slight increase in query complexity).
        RETURN QUERY
        EXECUTE 'SELECT b.authz_id FROM ' || actor_table || ' AS a
            JOIN ' || authz_table || ' AS b ON b.id = a.target
            JOIN ' || authz_table || ' AS c ON c.id = a.target
            WHERE c.authz_id = ANY ($1)
              AND a.authorizee = $2
              AND a.permission::text = $3::text
          UNION SELECT b.authz_id FROM ' || group_table || ' AS a
            JOIN groups_for_actor($2) AS gs(id) ON gs.id = a.authorizee
            JOIN ' || authz_table || ' AS b ON b.id = a.target
            JOIN ' || authz_table || ' AS c ON c.id = a.target
            WHERE c.authz_id = ANY ($1)
            AND a.permission::text = $3::text'
          USING query_targets, actor_id, perm;
END;
$$;

COMMIT;
