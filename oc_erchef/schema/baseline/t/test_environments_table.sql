CREATE OR REPLACE FUNCTION test_environments_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN QUERY SELECT has_table('environments');

  -- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('environments', 'id');
  RETURN QUERY SELECT col_is_pk('environments', 'id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('environments', 'authz_id', TRUE);
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('environments', 'org_id');
  RETURN QUERY SELECT chef_pgtap.col_is_name('environments', 'name');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('environments', 'last_updated_by');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('environments', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('environments', 'updated_at');
  RETURN QUERY SELECT chef_pgtap.col_is_blob('environments', 'serialized_object', TRUE); -- TODO: Make this NOT NULL

  RETURN QUERY SELECT col_is_unique('environments', ARRAY['org_id', 'name']);

  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('environments', 'environments_authz_id_key', 'authz_id');
  RETURN QUERY SELECT chef_pgtap.has_index('environments', 'environments_org_id_name_key', ARRAY['org_id', 'name']);

  -- Keys

  RETURN QUERY SELECT has_pk('environments');
  RETURN QUERY SELECT hasnt_fk('environments');

END;
$$;
