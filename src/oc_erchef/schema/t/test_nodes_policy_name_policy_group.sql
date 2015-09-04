CREATE OR REPLACE FUNCTION test_nodes_pname_pgroup()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN QUERY SELECT has_table('nodes');

  -- Columns

  RETURN QUERY SELECT col_type_is('nodes', 'policy_group', 'character varying(255)');
  RETURN QUERY SELECT col_type_is('nodes', 'policy_name', 'character varying(255)');

  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('nodes', 'nodes_policy_group', ARRAY['org_id', 'policy_group']);
  RETURN QUERY SELECT chef_pgtap.has_index('nodes', 'nodes_policy_name', ARRAY['org_id', 'policy_name']);

END;
$$;
