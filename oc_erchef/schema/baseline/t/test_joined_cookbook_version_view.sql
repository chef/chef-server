CREATE OR REPLACE FUNCTION test_joined_cookbook_version_view()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_view('joined_cookbook_version');

  -- Columns

  RETURN QUERY SELECT has_column('joined_cookbook_version', 'major');
  RETURN QUERY SELECT col_type_is('joined_cookbook_version', 'major', 'bigint');

  RETURN QUERY SELECT has_column('joined_cookbook_version', 'minor');
  RETURN QUERY SELECT col_type_is('joined_cookbook_version', 'minor', 'bigint');

  RETURN QUERY SELECT has_column('joined_cookbook_version', 'patch');
  RETURN QUERY SELECT col_type_is('joined_cookbook_version', 'patch', 'bigint');

  RETURN QUERY SELECT has_column('joined_cookbook_version', 'version');
  RETURN QUERY SELECT col_type_is('joined_cookbook_version', 'version', 'text');

  RETURN QUERY SELECT has_column('joined_cookbook_version', 'serialized_object');
  RETURN QUERY SELECT col_type_is('joined_cookbook_version', 'serialized_object', 'bytea');

  RETURN QUERY SELECT has_column('joined_cookbook_version', 'id');
  RETURN QUERY SELECT col_type_is('joined_cookbook_version', 'id', 'character(32)');

  RETURN QUERY SELECT has_column('joined_cookbook_version', 'org_id');
  RETURN QUERY SELECT col_type_is('joined_cookbook_version', 'org_id', 'character(32)');

  RETURN QUERY SELECT has_column('joined_cookbook_version', 'name');
  RETURN QUERY SELECT col_type_is('joined_cookbook_version', 'name', 'text');

END;
$$;
