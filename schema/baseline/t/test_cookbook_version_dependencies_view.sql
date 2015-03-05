CREATE OR REPLACE FUNCTION test_cookbook_version_dependencies_view()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_view('cookbook_version_dependencies');

  -- Columns

  RETURN QUERY SELECT has_column('cookbook_version_dependencies', 'org_id');
  RETURN QUERY SELECT col_type_is('cookbook_version_dependencies', 'org_id', 'character(32)');

  RETURN QUERY SELECT has_column('cookbook_version_dependencies', 'name');
  RETURN QUERY SELECT col_type_is('cookbook_version_dependencies', 'name', 'text');

  RETURN QUERY SELECT has_column('cookbook_version_dependencies', 'major');
  RETURN QUERY SELECT col_type_is('cookbook_version_dependencies', 'major', 'bigint');

  RETURN QUERY SELECT has_column('cookbook_version_dependencies', 'minor');
  RETURN QUERY SELECT col_type_is('cookbook_version_dependencies', 'minor', 'bigint');

  RETURN QUERY SELECT has_column('cookbook_version_dependencies', 'patch');
  RETURN QUERY SELECT col_type_is('cookbook_version_dependencies', 'patch', 'bigint');

  RETURN QUERY SELECT has_column('cookbook_version_dependencies', 'dependencies');
  RETURN QUERY SELECT col_type_is('cookbook_version_dependencies', 'dependencies', 'text');

END;
$$;
