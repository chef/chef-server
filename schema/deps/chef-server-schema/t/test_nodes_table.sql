CREATE OR REPLACE FUNCTION test_nodes_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN QUERY SELECT has_table('nodes');

  -- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('nodes', 'id');
  RETURN QUERY SELECT col_is_pk('nodes', 'id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('nodes', 'authz_id', TRUE);
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('nodes', 'org_id');
  RETURN QUERY SELECT chef_pgtap.col_is_name('nodes', 'name');
  RETURN QUERY SELECT chef_pgtap.col_is_name('nodes', 'environment');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('nodes', 'last_updated_by');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('nodes', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('nodes', 'updated_at');
  RETURN QUERY SELECT chef_pgtap.col_is_blob('nodes', 'serialized_object', TRUE); -- TODO: make this NOT NULL

  RETURN QUERY SELECT col_is_unique('nodes', ARRAY['org_id', 'name']);

  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('nodes', 'nodes_authz_id_key', 'authz_id');
  RETURN QUERY SELECT chef_pgtap.has_index('nodes', 'nodes_org_id_name_key', ARRAY['org_id', 'name']);
  RETURN QUERY SELECT chef_pgtap.has_index('nodes', 'nodes_org_id_environment_index', ARRAY['org_id', 'environment']);

  -- Keys

  RETURN QUERY SELECT has_pk('nodes');
  RETURN QUERY SELECT hasnt_fk('nodes');

END;
$$;
