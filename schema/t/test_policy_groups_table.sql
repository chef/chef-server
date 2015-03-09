
CREATE OR REPLACE FUNCTION test_policy_groups_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN QUERY SELECT has_table('policy_groups');
  RETURN QUERY SELECT col_type_is('policy_groups', 'name', 'character varying(255)');
  RETURN QUERY SELECT col_type_is('policy_groups', 'org_id', 'character(32)');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('policy_groups', 'authz_id', TRUE);
  RETURN QUERY SELECT col_is_pk('policy_groups', 'id');
  RETURN QUERY SELECT fk_ok('policy_groups', 'org_id', 'orgs', 'id');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('policy_groups', 'last_updated_by');
  RETURN QUERY SELECT chef_pgtap.has_index('policy_groups', 'policy_groups_org_id_name_unique', ARRAY['org_id', 'name']);
  RETURN QUERY SELECT chef_pgtap.has_index('policy_groups', 'policy_groups_org_id_authz_id_name_unique', ARRAY['org_id', 'authz_id', 'name']);
END;
$$
