CREATE OR REPLACE FUNCTION test_checksums_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('checksums');

  -- Columns
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('checksums', 'org_id');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('checksums', 'checksum');

  RETURN QUERY SELECT col_is_pk('checksums', ARRAY['org_id', 'checksum']);

  -- Keys
  RETURN QUERY SELECT has_pk('checksums');
  RETURN QUERY SELECT hasnt_fk('checksums');

END;
$$;
