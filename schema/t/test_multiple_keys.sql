CREATE OR REPLACE FUNCTION test_multiple_keys()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('keys');

  -- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('keys', 'id');
  RETURN QUERY SELECT chef_pgtap.col_is_name('keys', 'key_name');

  -- We could use a test for 'text' columns; alternately col_is_blob('keys', 'key', false) (but it isn't a blob)
  RETURN QUERY SELECT has_column('keys', 'public_key');
  RETURN QUERY SELECT col_not_null('keys', 'public_key');
  RETURN QUERY SELECT col_type_is('keys', 'public_key', 'text');
  RETURN QUERY SELECT col_hasnt_default('keys', 'public_key');

  RETURN QUERY SELECT has_column('keys', 'key_version');
  RETURN QUERY SELECT col_not_null('keys', 'key_version');
  RETURN QUERY SELECT col_type_is('keys', 'key_version', 'integer');

  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('keys', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('keys', 'expires_at');

  RETURN QUERY SELECT col_is_pk('keys', ARRAY['id', 'key_name']);

  -- Keys
  RETURN QUERY SELECT has_pk('keys');

  -- Todo 

END;
$$;
