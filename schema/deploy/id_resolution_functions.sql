-- Deploy id_resolution_functions
-- requires: base

BEGIN;

-- These are to help adding data in terms of Authz IDs
-- TODO: Consider views with rewrite rules instead?
CREATE OR REPLACE FUNCTION actor_id(auth_actor.authz_id%TYPE)
RETURNS auth_actor.id%TYPE
LANGUAGE SQL STABLE STRICT
AS $$
   SELECT id FROM auth_actor WHERE authz_id = $1;
$$;

CREATE OR REPLACE FUNCTION group_id(auth_group.authz_id%TYPE)
RETURNS auth_group.id%TYPE
LANGUAGE SQL STABLE STRICT
AS $$
   SELECT id FROM auth_group WHERE authz_id = $1;
$$;

CREATE OR REPLACE FUNCTION object_id(auth_object.authz_id%TYPE)
RETURNS auth_object.id%TYPE
LANGUAGE SQL STABLE STRICT
AS $$
   SELECT id FROM auth_object WHERE authz_id = $1;
$$;

CREATE OR REPLACE FUNCTION container_id(auth_container.authz_id%TYPE)
RETURNS auth_container.id%TYPE
LANGUAGE SQL STABLE STRICT
AS $$
   SELECT id FROM auth_container WHERE authz_id = $1;
$$;

CREATE OR REPLACE FUNCTION authz_id_for_type(char(32), auth_type)
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

COMMIT;
