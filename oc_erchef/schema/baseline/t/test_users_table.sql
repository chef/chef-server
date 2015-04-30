CREATE OR REPLACE FUNCTION test_users_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('osc_users');

-- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('osc_users', 'id');
  RETURN QUERY SELECT col_is_pk('osc_users', 'id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('osc_users', 'authz_id', TRUE);

  RETURN QUERY SELECT chef_pgtap.col_is_name('osc_users', 'username');
  RETURN QUERY SELECT col_is_unique('osc_users', 'username');

  RETURN QUERY SELECT has_column('osc_users', 'email');
  RETURN QUERY SELECT col_is_null('osc_users', 'email');
  RETURN QUERY SELECT col_is_unique('osc_users', 'email');
  RETURN QUERY SELECT col_type_is('osc_users', 'email', 'text');
  RETURN QUERY SELECT col_hasnt_default('osc_users', 'email');

  RETURN QUERY SELECT has_column('osc_users', 'hashed_password');
  RETURN QUERY SELECT col_not_null('osc_users', 'hashed_password');
  RETURN QUERY SELECT col_type_is('osc_users', 'hashed_password', 'text');
  RETURN QUERY SELECT col_hasnt_default('osc_users', 'hashed_password');

  RETURN QUERY SELECT has_column('osc_users', 'salt');
  RETURN QUERY SELECT col_not_null('osc_users', 'salt');
  RETURN QUERY SELECT col_type_is('osc_users', 'salt', 'text');
  RETURN QUERY SELECT col_hasnt_default('osc_users', 'salt');

  -- TODO: Constrain this?
  RETURN QUERY SELECT has_column('osc_users', 'hash_type');
  RETURN QUERY SELECT col_not_null('osc_users', 'hash_type');
  RETURN QUERY SELECT col_type_is('osc_users', 'hash_type', 'text');
  RETURN QUERY SELECT col_hasnt_default('osc_users', 'hash_type');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('osc_users', 'last_updated_by');

  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('osc_users', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('osc_users', 'updated_at');

  RETURN QUERY SELECT has_column('osc_users', 'external_authentication_uid');
  RETURN QUERY SELECT col_is_null('osc_users', 'external_authentication_uid');
  RETURN QUERY SELECT col_type_is('osc_users', 'external_authentication_uid', 'text');
  RETURN QUERY SELECT col_hasnt_default('osc_users', 'external_authentication_uid');

  RETURN QUERY SELECT has_column('osc_users', 'recovery_authentication_enabled');
  RETURN QUERY SELECT col_is_null('osc_users', 'recovery_authentication_enabled'); -- TODO: this should be NOT NULL, with a default
  RETURN QUERY SELECT col_type_is('osc_users', 'recovery_authentication_enabled', 'boolean');
  RETURN QUERY SELECT col_hasnt_default('osc_users', 'recovery_authentication_enabled'); -- TODO: default = false

  RETURN QUERY SELECT chef_pgtap.col_is_flag('osc_users', 'admin', FALSE);

  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('osc_users', 'osc_users_authz_id_key', 'authz_id');
  RETURN QUERY SELECT chef_pgtap.has_index('osc_users', 'osc_users_username_key', 'username');
  RETURN QUERY SELECT chef_pgtap.has_index('osc_users', 'osc_users_email_key', 'email');

  -- Keys

  RETURN QUERY SELECT has_pk('osc_users');
  RETURN QUERY SELECT hasnt_fk('osc_users');

END;
$$;
