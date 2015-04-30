CREATE OR REPLACE FUNCTION test_cookbooks_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('cookbooks');

  -- Columns

  RETURN QUERY SELECT has_column('cookbooks', 'id');
  RETURN QUERY SELECT col_not_null('cookbooks', 'id');
  RETURN QUERY SELECT col_type_is('cookbooks', 'id', 'integer');
  RETURN QUERY SELECT col_has_default('cookbooks', 'id');
  RETURN QUERY SELECT col_default_is('cookbooks', 'id', 'nextval(''cookbooks_id_seq''::regclass)');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('cookbooks', 'org_id');
  RETURN QUERY SELECT chef_pgtap.col_is_name('cookbooks', 'name');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('cookbooks', 'authz_id', TRUE);

  RETURN QUERY SELECT col_is_pk('cookbooks', 'id');

  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('cookbooks', 'cookbooks_authz_id_key', 'authz_id');
  RETURN QUERY SELECT chef_pgtap.has_index('cookbooks', 'cookbooks_org_id_name_key', ARRAY['org_id', 'name']);

  -- TODO: THIS INDEX IS SUPERFLUOUS
  RETURN QUERY SELECT chef_pgtap.has_index('cookbooks', 'cookbooks_org_id_index', 'org_id');

  -- Keys

  RETURN QUERY SELECT has_pk('cookbooks');
  RETURN QUERY SELECT hasnt_fk('cookbooks');

END;
$$;
