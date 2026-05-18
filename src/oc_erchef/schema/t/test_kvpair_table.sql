CREATE OR REPLACE FUNCTION test_kvpair_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('kvpair');

  RETURN QUERY SELECT is(
    (SELECT COUNT(*)::integer FROM kvpair WHERE key = 'itime'),
    1,
    'kvpair has exactly one itime row'
  );

  RETURN QUERY SELECT is(
    (
      SELECT to_timestamp(value::double precision)::timestamp without time zone
      FROM kvpair
      WHERE key = 'itime'
    ),
    (
      SELECT date_trunc('second', created_at)::timestamp without time zone
      FROM kvpair
      WHERE key = 'itime'
    ),
    'itime epoch text converts to timestamp matching created_at to the second'
  );

END;
$$;
