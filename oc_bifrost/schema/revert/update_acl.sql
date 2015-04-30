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

COMMIT;
