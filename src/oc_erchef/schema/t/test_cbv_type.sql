CREATE OR REPLACE FUNCTION test_cbv_type()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$

BEGIN
  RETURN QUERY SELECT has_type( 'cbv' );
  RETURN QUERY SELECT has_column( 'cbv', 'name');
  RETURN QUERY SELECT col_type_is('cbv', 'name', 'text');
  RETURN QUERY SELECT has_column( 'cbv', 'major');
  RETURN QUERY SELECT col_type_is('cbv', 'major', 'bigint');
  RETURN QUERY SELECT has_column( 'cbv', 'minor');
  RETURN QUERY SELECT col_type_is('cbv', 'minor', 'bigint');
  RETURN QUERY SELECT has_column( 'cbv', 'patch');
  RETURN QUERY SELECT col_type_is('cbv', 'patch', 'bigint');
END;

$$;
