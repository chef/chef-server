-- Deploy create_and_add_permissions
-- requires: base
-- requires: id_resolution_functions

BEGIN;

-- This is for creating an authz_entity, and also adding permissions on the entity
-- to an actor (presumably the requesting actor who created it).  This function will
-- put a newly created actor into its own ACL if the entity_type is actor.  Passing
-- a NULL creator_id is possible (e.g., in the case where this is something being
-- created by the superuser).
CREATE OR REPLACE FUNCTION create_and_add_permissions(
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

COMMIT;
