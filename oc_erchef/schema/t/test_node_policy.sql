
CREATE OR REPLACE FUNCTION test_node_policy_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN QUERY SELECT has_table('node_policy');
  RETURN QUERY SELECT col_type_is('node_policy', 'node_id', 'character(32)');
  RETURN QUERY SELECT col_type_is('node_policy', 'name', 'character varying(256)');
  RETURN QUERY SELECT col_type_is('node_policy', 'policy_group', 'character varying(256)');
  --RETURN QUERY SELECT fk_ok('node_policy', 'name', 'policies','name');
  --RETURN QUERY SELECT fk_ok('node_policy', 'policy_group', 'policies','policy_group');
  RETURN QUERY SELECT fk_ok('node_policy', 'node_id', 'nodes', 'id');
END;
$$
