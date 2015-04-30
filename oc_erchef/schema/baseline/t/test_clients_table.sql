CREATE OR REPLACE FUNCTION test_clients_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('clients');

  -- Columns
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('clients', 'id');
  RETURN QUERY SELECT col_is_pk('clients', 'id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('clients', 'org_id');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('clients', 'authz_id', TRUE);
  RETURN QUERY SELECT chef_pgtap.col_is_name('clients', 'name');

  -- TODO: This column's tests could technically be done by
  -- chef_pgtap.col_is_name, but it's not really a name, now, is it?
  RETURN QUERY SELECT has_column('clients', 'public_key');
  RETURN QUERY SELECT col_is_null('clients', 'public_key');
  RETURN QUERY SELECT col_type_is('clients', 'public_key', 'text');
  RETURN QUERY SELECT col_hasnt_default('clients', 'public_key');

  RETURN QUERY SELECT has_column('clients', 'validator');
  RETURN QUERY SELECT col_not_null('clients', 'validator');
  RETURN QUERY SELECT col_type_is('clients', 'validator', 'boolean');
  RETURN QUERY SELECT col_hasnt_default('clients', 'validator');

  -- I wanted to do this:
  --
  -- RETURN QUERY SELECT chef_pgtap.col_is_flag('clients', 'validator', FALSE);
  --
  -- But the column doesn't have a default value at the moment.
  --
  --        ;_;

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('clients', 'last_updated_by');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('clients', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('clients', 'updated_at');

  RETURN QUERY SELECT has_column('clients', 'pubkey_version');
  RETURN QUERY SELECT col_not_null('clients', 'pubkey_version');
  RETURN QUERY SELECT col_type_is('clients', 'pubkey_version', 'smallint');
  RETURN QUERY SELECT col_hasnt_default('clients', 'pubkey_version');

  RETURN QUERY SELECT chef_pgtap.col_is_flag('clients', 'admin', FALSE);

  RETURN QUERY SELECT col_is_unique('clients', ARRAY['org_id', 'name']);

  -- Indexes
  RETURN QUERY SELECT chef_pgtap.has_index('clients', 'clients_authz_id_key', 'authz_id');
  RETURN QUERY SELECT chef_pgtap.has_index('clients',
                                        'org_id_name_unique',
                                        ARRAY['org_id', 'name']);

  -- Keys
  RETURN QUERY SELECT has_pk('clients');
  RETURN QUERY SELECT hasnt_fk('clients');

END;
$$;
