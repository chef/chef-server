CREATE OR REPLACE FUNCTION test_opc_customers_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('opc_customers');

  -- Columns

  RETURN QUERY SELECT has_column('opc_customers', 'id');
  RETURN QUERY SELECT col_is_pk('opc_customers', 'id');
  RETURN QUERY SELECT col_not_null('opc_customers', 'id');
  RETURN QUERY SELECT col_type_is('opc_customers', 'id', 'integer');
  RETURN QUERY SELECT col_has_default('opc_customers', 'id');
  RETURN QUERY SELECT col_default_is('opc_customers', 'id', 'nextval(''opc_customers_id_seq''::regclass)');

  RETURN QUERY SELECT chef_pgtap.col_is_name('opc_customers', 'name');
  RETURN QUERY SELECT col_is_unique('opc_customers', 'name');

  RETURN QUERY SELECT chef_pgtap.col_is_name('opc_customers', 'display_name');

  RETURN QUERY SELECT has_column('opc_customers', 'domain');
  RETURN QUERY SELECT col_is_unique('opc_customers', 'domain');
  RETURN QUERY SELECT col_is_null('opc_customers', 'domain');
  RETURN QUERY SELECT col_type_is('opc_customers', 'domain', 'text');
  RETURN QUERY SELECT col_hasnt_default('opc_customers', 'domain');

  RETURN QUERY SELECT has_column('opc_customers', 'contact');
  RETURN QUERY SELECT col_not_null('opc_customers', 'contact');
  RETURN QUERY SELECT col_type_is('opc_customers', 'contact', 'text');
  RETURN QUERY SELECT col_hasnt_default('opc_customers', 'contact');

  RETURN QUERY SELECT has_column('opc_customers', 'priority');
  RETURN QUERY SELECT col_not_null('opc_customers', 'priority');
  RETURN QUERY SELECT col_type_is('opc_customers', 'priority', 'integer');
  RETURN QUERY SELECT col_has_default('opc_customers', 'priority');
  RETURN QUERY SELECT col_default_is('opc_customers', 'priority', '0');

  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('opc_customers', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('opc_customers', 'updated_at');

  RETURN QUERY SELECT chef_pgtap.col_is_flag('opc_customers', 'osc_customer', FALSE);
  RETURN QUERY SELECT chef_pgtap.col_is_flag('opc_customers', 'ohc_customer', FALSE);
  RETURN QUERY SELECT chef_pgtap.col_is_flag('opc_customers', 'opc_customer', FALSE);

  RETURN QUERY SELECT has_column('opc_customers', 'support_plan');
  RETURN QUERY SELECT col_not_null('opc_customers', 'support_plan');
  RETURN QUERY SELECT col_type_is('opc_customers', 'support_plan', 'text');
  RETURN QUERY SELECT col_has_default('opc_customers', 'support_plan');
  RETURN QUERY SELECT col_default_is('opc_customers', 'support_plan', '');

  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('opc_customers', 'opc_customers_domain_key', 'domain');
  RETURN QUERY SELECT chef_pgtap.has_index('opc_customers', 'opc_customers_name_key', 'name');

  -- Keys

  RETURN QUERY SELECT has_pk('opc_customers');
  RETURN QUERY SELECT hasnt_fk('opc_customers');

END;
$$;
