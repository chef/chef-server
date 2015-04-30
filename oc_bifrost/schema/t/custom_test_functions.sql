CREATE SCHEMA opscode;

CREATE OR REPLACE FUNCTION opscode._table_permissions(p_table_schema NAME, p_role_name NAME, p_permissions TEXT[], p_table_type TEXT)
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
-- DECLARE
--    all_permissions CONSTANT TEXT[] := {'SELECT', 'INSERT', 'UPDATE','DELETE', 'TRUNCATE', 'REFERENCES', 'TRIGGER'};
--    non_granted_permissions CONSTANT TEXT[] :=
BEGIN
   RETURN QUERY
   SELECT table_privs_are(p_table_schema, table_name, p_role_name, p_permissions,
   'Role ' || p_role_name || ' should be granted ' || array_to_string(p_permissions, ', ') || ' on ' || CASE p_table_type WHEN 'BASE TABLE' THEN 'table ' WHEN 'VIEW' THEN 'view ' END || p_table_schema || '.' || table_name)
   FROM information_schema.tables
   WHERE table_schema = p_table_schema
     AND table_type = p_table_type;
END
$$;

CREATE OR REPLACE FUNCTION opscode.all_table_permissions(p_table_schema NAME, p_role_name NAME, p_permissions TEXT[])
RETURNS SETOF TEXT
LANGUAGE SQL
AS $$
   SELECT opscode._table_permissions($1, $2, $3, 'BASE TABLE')
$$;

CREATE OR REPLACE FUNCTION opscode.all_view_permissions(p_table_schema NAME, p_role_name NAME, p_permissions TEXT[])
RETURNS SETOF TEXT
LANGUAGE SQL
AS $$
   SELECT opscode._table_permissions($1, $2, $3, 'VIEW')
$$;
