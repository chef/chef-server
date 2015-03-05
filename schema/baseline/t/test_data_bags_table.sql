CREATE OR REPLACE FUNCTION test_data_bags_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('data_bags');

  -- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('data_bags', 'id');
  RETURN QUERY SELECT col_is_pk('data_bags', 'id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('data_bags', 'authz_id', TRUE);
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('data_bags', 'org_id');
  RETURN QUERY SELECT chef_pgtap.col_is_name('data_bags', 'name');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('data_bags', 'last_updated_by');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('data_bags', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('data_bags', 'updated_at');

  RETURN QUERY SELECT col_is_unique('data_bags', ARRAY['org_id', 'name']);

  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('data_bags', 'data_bags_authz_id_key', 'authz_id');
  RETURN QUERY SELECT chef_pgtap.has_index('data_bags', 'data_bags_org_id_name_key', ARRAY['org_id', 'name']);

  -- Keys

  RETURN QUERY SELECT has_pk('data_bags');
  RETURN QUERY SELECT hasnt_fk('data_bags');

END;
$$;
