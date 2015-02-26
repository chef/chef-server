CREATE OR REPLACE FUNCTION test_sandboxed_checksums()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('sandboxed_checksums');

  -- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('sandboxed_checksums', 'org_id');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('sandboxed_checksums', 'sandbox_id');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('sandboxed_checksums', 'checksum');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('sandboxed_checksums', 'created_at');

  RETURN QUERY SELECT col_is_pk('sandboxed_checksums', ARRAY['sandbox_id', 'org_id', 'checksum']);

  -- Keys

  RETURN QUERY SELECT has_pk('sandboxed_checksums');
  RETURN QUERY SELECT hasnt_fk('sandboxed_checksums');

END;
$$;
