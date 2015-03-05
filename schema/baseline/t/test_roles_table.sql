CREATE OR REPLACE FUNCTION test_roles_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('roles');

  -- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('roles', 'id');
  RETURN QUERY SELECT col_is_pk('roles', 'id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('roles', 'authz_id', TRUE);
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('roles', 'org_id');
  RETURN QUERY SELECT chef_pgtap.col_is_name('roles', 'name');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('roles', 'last_updated_by');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('roles', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('roles', 'updated_at');
  RETURN QUERY SELECT chef_pgtap.col_is_blob('roles', 'serialized_object', TRUE); -- TODO: make this NOT NULL

  RETURN QUERY SELECT col_is_unique('roles', ARRAY['org_id', 'name']);

  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('roles', 'roles_authz_id_key', 'authz_id');
  RETURN QUERY SELECT chef_pgtap.has_index('roles', 'roles_org_id_name_key', ARRAY['org_id', 'name']);

  -- Keys

  RETURN QUERY SELECT has_pk('roles');
  RETURN QUERY SELECT hasnt_fk('roles');

END;
$$;
