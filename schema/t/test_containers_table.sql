CREATE OR REPLACE FUNCTION test_containers_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('containers');

  -- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('containers', 'id');
  RETURN QUERY SELECT col_is_pk('containers', 'id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('containers', 'authz_id', TRUE);
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('containers', 'org_id');

  RETURN QUERY SELECT chef_pgtap.col_is_name('containers', 'name');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('containers', 'last_updated_by');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('containers', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('containers', 'updated_at');

  RETURN QUERY SELECT col_is_unique('containers', ARRAY['org_id', 'name']);

  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('containers', 'containers_authz_id_key', 'authz_id');
  RETURN QUERY SELECT chef_pgtap.has_index('containers', 'containers_org_id_name_key', ARRAY['org_id', 'name']);

  -- Keys

  RETURN QUERY SELECT has_pk('containers');
  RETURN QUERY SELECT hasnt_fk('containers');

END;
$$;
