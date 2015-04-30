
CREATE OR REPLACE FUNCTION test_policy_revisions_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN QUERY SELECT has_table('policy_revisions');

  RETURN QUERY SELECT col_is_pk('policy_revisions', 'id');
  RETURN QUERY SELECT col_type_is('policy_revisions', 'name', 'character varying(255)');
  RETURN QUERY SELECT col_type_is('policy_revisions', 'org_id', 'character(32)');
  RETURN QUERY SELECT col_type_is('policy_revisions', 'serialized_object', 'bytea');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('policy_revisions', 'last_updated_by');

  RETURN QUERY SELECT chef_pgtap.has_index('policy_revisions', 'policy_revisions_org_id_name_revision_id_unique', ARRAY['org_id', 'name', 'revision_id']);
  RETURN QUERY SELECT col_is_unique('policy_revisions', ARRAY['org_id', 'name', 'revision_id']);

  RETURN QUERY SELECT fk_ok('policy_revisions', 'org_id', 'orgs', 'id');
  RETURN QUERY SELECT fk_ok('policy_revisions', ARRAY['org_id', 'policy_authz_id', 'name'],
             'policies', ARRAY['org_id', 'authz_id', 'name']);

END;
$$
