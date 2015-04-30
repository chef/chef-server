CREATE OR REPLACE FUNCTION test_cookbook_versions_by_rank_view()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_view('cookbook_versions_by_rank');

  -- Columns

  RETURN QUERY SELECT has_column('cookbook_versions_by_rank', 'major');
  RETURN QUERY SELECT col_type_is('cookbook_versions_by_rank', 'major', 'bigint');

  RETURN QUERY SELECT has_column('cookbook_versions_by_rank', 'minor');
  RETURN QUERY SELECT col_type_is('cookbook_versions_by_rank', 'minor', 'bigint');

  RETURN QUERY SELECT has_column('cookbook_versions_by_rank', 'patch');
  RETURN QUERY SELECT col_type_is('cookbook_versions_by_rank', 'patch', 'bigint');

  RETURN QUERY SELECT has_column('cookbook_versions_by_rank', 'version');
  RETURN QUERY SELECT col_type_is('cookbook_versions_by_rank', 'version', 'text');

  RETURN QUERY SELECT has_column('cookbook_versions_by_rank', 'serialized_object');
  RETURN QUERY SELECT col_type_is('cookbook_versions_by_rank', 'serialized_object', 'bytea');

  RETURN QUERY SELECT has_column('cookbook_versions_by_rank', 'org_id');
  RETURN QUERY SELECT col_type_is('cookbook_versions_by_rank', 'org_id', 'character(32)');

  RETURN QUERY SELECT has_column('cookbook_versions_by_rank', 'name');
  RETURN QUERY SELECT col_type_is('cookbook_versions_by_rank', 'name', 'text');

  RETURN QUERY SELECT has_column('cookbook_versions_by_rank', 'rank');
  RETURN QUERY SELECT col_type_is('cookbook_versions_by_rank', 'rank', 'bigint');

END;
$$;
