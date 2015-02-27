CREATE OR REPLACE FUNCTION test_org_migration_state_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('org_migration_state');

  -- Columns

  RETURN QUERY SELECT has_column('org_migration_state', 'id');
  RETURN QUERY SELECT has_column('org_migration_state', 'org_name', 'text');
  RETURN QUERY SELECT has_column('org_migration_state', 'org_id');
  RETURN QUERY SELECT has_column('org_migration_state', 'state');
  RETURN QUERY SELECT has_column('org_migration_state', 'fail_location');
  RETURN QUERY SELECT has_column('org_migration_state', 'migration_start');
  RETURN QUERY SELECT has_column('org_migration_state', 'migration_end');
  RETURN QUERY SELECT has_column('org_migration_state', 'migration_type');
  RETURN QUERY SELECT col_not_null('org_migration_state', 'org_name');
  RETURN QUERY SELECT col_not_null('org_migration_state', 'org_id');
  RETURN QUERY SELECT col_not_null('org_migration_state', 'state');

  RETURN QUERY SELECT col_type_is('org_migration_state', 'id', 'integer');
  RETURN QUERY SELECT col_type_is('org_migration_state', 'org_name', 'text');
  RETURN QUERY SELECT col_type_is('org_migration_state', 'org_id', 'character varying(36)');
  RETURN QUERY SELECT col_type_is('org_migration_state', 'state', 'org_state');
  RETURN QUERY SELECT col_type_is('org_migration_state', 'fail_location', 'character varying(50)');
  RETURN QUERY SELECT col_type_is('org_migration_state', 'migration_start', 'timestamp without time zone');
  RETURN QUERY SELECT col_type_is('org_migration_state', 'migration_end', 'timestamp without time zone');
  RETURN QUERY SELECT has_column('org_migration_state', 'migration_type', 'text');

  RETURN QUERY SELECT col_has_default('org_migration_state', 'state');
  RETURN QUERY SELECT col_default_is('org_migration_state', 'state', 'holding');

  -- Keys
  RETURN QUERY SELECT has_pk('org_migration_state', 'org_id, migration_type');
  RETURN QUERY SELECT hasnt_fk('org_migration_state');

END;
$$;
