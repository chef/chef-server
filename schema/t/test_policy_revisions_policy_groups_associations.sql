
CREATE OR REPLACE FUNCTION test_prpga_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN QUERY SELECT has_table('policy_revisions_policy_groups_association');

  RETURN QUERY SELECT col_is_pk('policy_revisions_policy_groups_association', 'id');
  RETURN QUERY SELECT col_type_is('policy_revisions_policy_groups_association', 'org_id', 'character(32)');
  RETURN QUERY SELECT col_type_is('policy_revisions_policy_groups_association', 'policy_revision_name', 'character varying(255)');
  RETURN QUERY SELECT col_type_is('policy_revisions_policy_groups_association', 'policy_revision_revision_id', 'character varying(255)');
  RETURN QUERY SELECT col_type_is('policy_revisions_policy_groups_association', 'policy_group_name', 'character varying(255)');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('policy_revisions_policy_groups_association', 'last_updated_by');

  RETURN QUERY SELECT
    chef_pgtap.has_index('policy_revisions_policy_groups_association',
      'prpga_name_group_unique', ARRAY['org_id', 'policy_revision_name',
      'policy_group_name']);

  RETURN QUERY SELECT col_is_unique('policy_revisions_policy_groups_association', ARRAY['org_id', 'policy_revision_name', 'policy_group_name']);

  RETURN QUERY SELECT fk_ok('policy_revisions_policy_groups_association', 'org_id', 'orgs', 'id');
  RETURN QUERY SELECT fk_ok('policy_revisions_policy_groups_association', ARRAY['org_id', 'policy_group_authz_id', 'policy_group_name'],
             'policy_groups', ARRAY['org_id', 'authz_id', 'name']);

  RETURN QUERY SELECT fk_ok('policy_revisions_policy_groups_association', ARRAY['org_id', 'policy_revision_name', 'policy_revision_revision_id'],
             'policy_revisions', ARRAY['org_id', 'name', 'revision_id']);

END;
$$
