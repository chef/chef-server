CREATE OR REPLACE FUNCTION test_orgs_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('orgs');

  -- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('orgs', 'id');
  RETURN QUERY SELECT col_is_pk('orgs', 'id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('orgs', 'authz_id', TRUE);

  RETURN QUERY SELECT has_column('orgs', 'name');
  RETURN QUERY SELECT col_not_null('orgs', 'name');
  RETURN QUERY SELECT col_type_is('orgs', 'name', 'text');
  RETURN QUERY SELECT col_hasnt_default('orgs', 'name');
  RETURN QUERY SELECT col_is_unique('orgs', 'name');

  RETURN QUERY SELECT has_column('orgs', 'full_name');
  RETURN QUERY SELECT col_not_null('orgs', 'full_name');
  RETURN QUERY SELECT col_type_is('orgs', 'full_name', 'text');
  RETURN QUERY SELECT col_hasnt_default('orgs', 'full_name');

  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('orgs', 'assigned_at');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('orgs', 'last_updated_by', FALSE);
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('orgs', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('orgs', 'updated_at');

  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('orgs', 'orgs_authz_id_key', 'authz_id');

  -- Keys

  RETURN QUERY SELECT has_pk('orgs');
  RETURN QUERY SELECT hasnt_fk('orgs');

END;
$$;
