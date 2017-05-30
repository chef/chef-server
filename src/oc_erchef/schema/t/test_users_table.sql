-- Note that this replaces the 'test_users_table' function from the
-- Open Source schema tests, making it appropriate for Enterprise
-- Chef.

CREATE OR REPLACE FUNCTION test_users_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('users');

  -- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('users', 'id');
  RETURN QUERY SELECT col_is_pk('users', 'id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('users', 'authz_id', TRUE);

  RETURN QUERY SELECT chef_pgtap.col_is_name('users', 'username');
  RETURN QUERY SELECT col_is_unique('users', 'username');

  RETURN QUERY SELECT has_column('users', 'email');
  RETURN QUERY SELECT col_is_null('users', 'email');
  RETURN QUERY SELECT col_is_unique('users', 'email');
  RETURN QUERY SELECT col_type_is('users', 'email', 'text');
  RETURN QUERY SELECT col_hasnt_default('users', 'email');

  RETURN QUERY SELECT has_column('users', 'pubkey_version');
  RETURN QUERY SELECT col_not_null('users', 'pubkey_version');
  RETURN QUERY SELECT col_type_is('users', 'pubkey_version', 'integer'); -- TODO: different from clients; extract this test into something common
  RETURN QUERY SELECT col_hasnt_default('users', 'pubkey_version');

  RETURN QUERY SELECT has_column('users', 'public_key');
  RETURN QUERY SELECT col_is_null('users', 'public_key');
  RETURN QUERY SELECT col_type_is('users', 'public_key', 'text');
  RETURN QUERY SELECT col_hasnt_default('users', 'public_key');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('users', 'last_updated_by');

  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('users', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('users', 'updated_at');

  RETURN QUERY SELECT has_column('users', 'external_authentication_uid');
  RETURN QUERY SELECT col_is_null('users', 'external_authentication_uid');
  RETURN QUERY SELECT col_type_is('users', 'external_authentication_uid', 'text');
  RETURN QUERY SELECT col_hasnt_default('users', 'external_authentication_uid');

  RETURN QUERY SELECT has_column('users', 'recovery_authentication_enabled');
  RETURN QUERY SELECT col_is_null('users', 'recovery_authentication_enabled'); -- TODO: this should be NOT NULL, with a default
  RETURN QUERY SELECT col_type_is('users', 'recovery_authentication_enabled', 'boolean');
  RETURN QUERY SELECT col_hasnt_default('users', 'recovery_authentication_enabled'); -- TODO: default = false

  RETURN QUERY SELECT chef_pgtap.col_is_flag('users', 'admin', FALSE);

  RETURN QUERY SELECT has_column('users', 'hashed_password');
  RETURN QUERY SELECT col_is_null('users', 'hashed_password');
  RETURN QUERY SELECT col_type_is('users', 'hashed_password', 'text');
  RETURN QUERY SELECT col_hasnt_default('users', 'hashed_password');

  RETURN QUERY SELECT has_column('users', 'salt');
  RETURN QUERY SELECT col_is_null('users', 'salt');
  RETURN QUERY SELECT col_type_is('users', 'salt', 'text');
  RETURN QUERY SELECT col_hasnt_default('users', 'salt');

  RETURN QUERY SELECT has_column('users', 'hash_type');
  RETURN QUERY SELECT col_is_null('users', 'hash_type');
  RETURN QUERY SELECT col_type_is('users', 'hash_type', 'password_hash_type');
  RETURN QUERY SELECT col_hasnt_default('users', 'hash_type');

  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('users', 'users_authz_id_key', 'authz_id');
  RETURN QUERY SELECT chef_pgtap.has_index('users', 'users_username_key', 'username');
  RETURN QUERY SELECT chef_pgtap.has_index('users', 'users_email_key', 'email');
  RETURN QUERY SELECT chef_pgtap.has_index('users', 'users_lower_email_idx', 'lower(email)');

  -- Keys

  RETURN QUERY SELECT has_pk('users');
  RETURN QUERY SELECT hasnt_fk('users');

  -- Constraints
  RETURN QUERY SELECT col_has_check('users', ARRAY['hashed_password', 'salt', 'hash_type']);

  -- Value constraint checks
    PREPARE password_hash_deps_check_1 AS
    INSERT INTO USERS (id, authz_id, username, last_updated_by, created_at, updated_at, pubkey_version, hashed_password)
               VALUES ('a', 'a', 'a', 'a', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, 0, 'SomeValueForPassword');

    PREPARE password_hash_deps_check_2 AS
    INSERT INTO USERS (id, authz_id, username, last_updated_by, created_at, updated_at, pubkey_version, hash_type)
               VALUES ('a', 'a', 'a', 'a', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, 0, 'SHA1');

    PREPARE password_hash_deps_check_3 AS
    INSERT INTO USERS (id, authz_id, username, last_updated_by, created_at, updated_at, pubkey_version, salt)
               VALUES ('a', 'a', 'a', 'a', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, 0, 'SomeSaltyGoodness');

    PREPARE password_hash_deps_check_4 AS
    INSERT INTO USERS (id, authz_id, username, last_updated_by, created_at, updated_at, pubkey_version, hashed_password, salt)
               VALUES ('a', 'a', 'a', 'a', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, 0, 'Something Hashed', 'SomeSaltyGoodness');

    PREPARE password_hash_deps_check_5 AS
    INSERT INTO USERS (id, authz_id, username, last_updated_by, created_at, updated_at, pubkey_version, hashed_password, hash_type, salt)
               VALUES ('a', 'a', 'a', 'a', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, 0, 'Something Hashed', 'SHA1', 'SomeSaltyGoodness');

   -- new row for relation "users" violates check constraint "password_hash_deps"
   RETURN QUERY SELECT throws_ok('password_hash_deps_check_1', '23514', NULL, 'hashedpassword is not accepted without hashtype and salt');
   RETURN QUERY SELECT throws_ok('password_hash_deps_check_2', '23514', NULL, 'hashtype is not accepted without hashedpassword and salt');
   RETURN QUERY SELECT throws_ok('password_hash_deps_check_3', '23514', NULL, 'salt is not accepted without hashtype and hashedpassword');
   RETURN QUERY SELECT throws_ok('password_hash_deps_check_4', '23514', NULL, 'hashedpassword and salt are not accepted without hashtype');
   RETURN QUERY SELECT lives_ok('password_hash_deps_check_5', 'salt, hashtype, and hashedpassword are accepted together');
END;
$$;
