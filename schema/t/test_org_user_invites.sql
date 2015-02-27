CREATE OR REPLACE FUNCTION test_org_user_invites_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('org_user_invites');

  -- Columns
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('org_user_invites', 'id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('org_user_invites', 'org_id');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('org_user_invites', 'user_id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('org_user_invites', 'last_updated_by', FALSE);
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('org_user_invites', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('org_user_invites', 'updated_at');

  -- Indexes

  -- Keys

  RETURN QUERY SELECT has_pk('org_user_invites');

  RETURN QUERY SELECT fk_ok('org_user_invites',
                            'org_id',
                            'orgs',
                            'id');

  RETURN QUERY SELECT chef_pgtap.fk_update_action_is('org_user_invites_org_id_fkey', 'NO ACTION');
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('org_user_invites_org_id_fkey', 'CASCADE');

  RETURN QUERY SELECT fk_ok('org_user_invites',
                            'user_id',
                            'users',
                            'id');

  RETURN QUERY SELECT chef_pgtap.fk_update_action_is('org_user_invites_user_id_fkey', 'NO ACTION');
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('org_user_invites_user_id_fkey', 'CASCADE');

END;
$$;
