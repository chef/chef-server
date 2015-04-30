CREATE OR REPLACE FUNCTION test_cookbook_artifacts_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('cookbook_artifacts');

  -- Columns
  RETURN QUERY SELECT columns_are('cookbook_artifacts', ARRAY['id',
                                                              'org_id',
                                                              'name',
                                                              'authz_id']);
  RETURN QUERY SELECT col_not_null('cookbook_artifacts', 'id');
  RETURN QUERY SELECT col_type_is('cookbook_artifacts', 'id', 'integer');
  RETURN QUERY SELECT col_has_default('cookbook_artifacts', 'id');
  RETURN QUERY SELECT col_default_is('cookbook_artifacts', 'id', 'nextval(''cookbook_artifacts_id_seq''::regclass)');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('cookbook_artifacts', 'org_id');
  RETURN QUERY SELECT chef_pgtap.col_is_name('cookbook_artifacts', 'name');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('cookbook_artifacts', 'authz_id', TRUE);

  RETURN QUERY SELECT col_is_pk('cookbooks', 'id');

  -- Indexes
  RETURN QUERY SELECT chef_pgtap.has_index('cookbook_artifacts', 'cookbook_artifacts_authz_id_key', 'authz_id');
  RETURN QUERY SELECT chef_pgtap.has_index('cookbook_artifacts', 'cookbook_artifacts_org_id_name_key', ARRAY['org_id', 'name']);

  -- Keys
  RETURN QUERY SELECT has_pk('cookbook_artifacts');
  RETURN QUERY SELECT hasnt_fk('cookbook_artifacts');

END;
$$;
