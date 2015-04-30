CREATE OR REPLACE FUNCTION test_opc_users_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('opc_users');

  -- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('opc_users', 'user_id');

  RETURN QUERY SELECT has_column('opc_users', 'customer_id');
  RETURN QUERY SELECT col_is_null('opc_users', 'customer_id'); -- TODO: nullable?  really?
  RETURN QUERY SELECT col_type_is('opc_users', 'customer_id', 'integer');
  RETURN QUERY SELECT col_hasnt_default('opc_users', 'customer_id');

  RETURN QUERY SELECT col_is_unique('opc_users', ARRAY['user_id', 'customer_id']);

  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('opc_users', 'opc_users_user_id_customer_id_key', ARRAY['user_id', 'customer_id']);

  -- Keys

  -- TODO: NO PRIMARY KEY!
  RETURN QUERY SELECT hasnt_pk('opc_users');

  -- TODO: This doesn't work for some reason.. suspect because there's no PK?
  --   SELECT has_fk('opc_users');

  RETURN QUERY SELECT fk_ok('opc_users', 'customer_id',
                            'opc_customers', 'id');
  RETURN QUERY SELECT chef_pgtap.fk_update_action_is('opc_users_customer_id_fkey', 'NO ACTION');
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('opc_users_customer_id_fkey', 'CASCADE');

  RETURN QUERY SELECT fk_ok('opc_users', 'user_id',
                            'users', 'id');
  RETURN QUERY SELECT chef_pgtap.fk_update_action_is('opc_users_user_id_fkey', 'NO ACTION');
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('opc_users_user_id_fkey', 'CASCADE');

  RETURN QUERY SELECT chef_pgtap.has_index('opc_users', 'opc_users_customer_id_index', 'customer_id');

END;
$$;
