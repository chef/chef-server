CREATE OR REPLACE FUNCTION test_groups_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('groups');

  -- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('groups', 'id');
  RETURN QUERY SELECT col_is_pk('groups', 'id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('groups', 'authz_id', TRUE);
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('groups', 'org_id');

  RETURN QUERY SELECT chef_pgtap.col_is_name('groups', 'name');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('groups', 'last_updated_by');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('groups', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('groups', 'updated_at');

  RETURN QUERY SELECT col_is_unique('groups', ARRAY['org_id', 'name']);

  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('groups', 'groups_authz_id_key', 'authz_id');
  RETURN QUERY SELECT chef_pgtap.has_index('groups', 'groups_org_id_name_key', ARRAY['org_id', 'name']);

  -- Keys

  RETURN QUERY SELECT has_pk('groups');
  RETURN QUERY SELECT hasnt_fk('groups');

END;
$$;
