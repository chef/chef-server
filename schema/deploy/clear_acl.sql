-- Deploy clear_acl
-- requires: base
-- requires: id_resolution_functions

BEGIN;

-- Function for clearing ACLs for arbitrary entities
CREATE OR REPLACE FUNCTION clear_acl(
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

COMMIT;
