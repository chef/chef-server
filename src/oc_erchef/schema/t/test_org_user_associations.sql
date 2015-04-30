CREATE OR REPLACE FUNCTION test_org_user_associations_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('org_user_associations');

  -- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('org_user_associations', 'org_id');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('org_user_associations', 'user_id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('org_user_associations', 'last_updated_by', FALSE);
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('org_user_associations', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('org_user_associations', 'updated_at');

  RETURN QUERY SELECT col_is_pk('org_user_associations', ARRAY['org_id', 'user_id']);

  -- Indexes

  -- Keys

  RETURN QUERY SELECT has_pk('org_user_associations');

  RETURN QUERY SELECT fk_ok('org_user_associations',
                            'org_id',
                            'orgs',
                            'id');

  RETURN QUERY SELECT chef_pgtap.fk_update_action_is('org_user_associations_org_id_fkey', 'NO ACTION');
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('org_user_associations_org_id_fkey', 'CASCADE');

  RETURN QUERY SELECT fk_ok('org_user_associations',
                            'user_id',
                            'users',
                            'id');

  RETURN QUERY SELECT chef_pgtap.fk_update_action_is('org_user_associations_user_id_fkey', 'NO ACTION');
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('org_user_associations_user_id_fkey', 'CASCADE');


END;
$$;
