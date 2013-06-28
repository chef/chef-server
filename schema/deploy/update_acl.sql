-- Deploy update_acl
-- requires: base
-- requires: id_resolution_functions

BEGIN;

-- Function for updating ACLs with arbitrary sets of groups and actors
CREATE OR REPLACE FUNCTION update_acl(
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

        -- hold the value of an authz id as we iterate through actors and groups
        id char(32);
BEGIN
        -- Clear out the old
        EXECUTE 'DELETE FROM ' || actor_table || '
            WHERE target = $1 AND permission = $2' USING target_id, perm;
        EXECUTE 'DELETE FROM ' || group_table || '
            WHERE target = $1 AND permission = $2' USING target_id, perm;

        -- Note that if any element of the actors or groups arrays are
        -- not a legitimate actor or group ID, an not-null constraint
        -- violation error for the "authorizee" column will occur when
        -- trying to insert that bogus value.
        --
        -- If all values are legit, then no exception will be raised,
        -- and the function will return TRUE.

        -- Insert the new actors
        IF actors IS NOT NULL THEN
          FOREACH id IN ARRAY actors LOOP
            EXECUTE 'INSERT INTO ' || actor_table || '(target, authorizee, permission)
              VALUES ($1, $2, $3)' USING target_id, actor_id(id), perm;
          END LOOP;
        END IF;

        -- Insert the new groups
        IF groups IS NOT NULL THEN
          FOREACH id IN ARRAY groups LOOP
            EXECUTE 'INSERT INTO ' || group_table || '(target, authorizee, permission)
              VALUES ($1, $2, $3)' USING target_id, group_id(id), perm;
          END LOOP;
        END IF;

       RETURN TRUE;
END;
$$;

COMMIT;
