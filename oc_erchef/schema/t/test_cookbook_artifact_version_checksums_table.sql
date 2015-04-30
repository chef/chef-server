CREATE OR REPLACE FUNCTION test_cookbook_artifact_version_checksums_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('cookbook_artifact_version_checksums');

  -- Columns
  RETURN QUERY SELECT columns_are('cookbook_artifact_version_checksums', ARRAY['cookbook_artifact_version_id',
                                                                               'org_id',
                                                                               'checksum']);

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('cookbook_artifact_version_checksums', 'org_id');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('cookbook_artifact_version_checksums', 'checksum');

  -- Indexes
  RETURN QUERY SELECT chef_pgtap.has_index('cookbook_artifact_version_checksums',
                                           'cookbook_artifact_version_checksums_by_id',
                                           'cookbook_artifact_version_id');
  RETURN QUERY SELECT chef_pgtap.has_index('cookbook_artifact_version_checksums',
                                           'cookbook_artifact_version_checksums_by_org_id_checksum',
                                           ARRAY['org_id', 'checksum']);

  -- Keys
  RETURN QUERY SELECT fk_ok('cookbook_artifact_version_checksums', 'cookbook_artifact_version_id',
                            'cookbook_artifact_versions', 'id');
  RETURN QUERY SELECT chef_pgtap.fk_update_action_is('cookbook_artifact_version_checksums_cookbook_artifact_version_id_fkey', 'NO ACTION');
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('cookbook_artifact_version_checksums_cookbook_artifact_version_id_fkey', 'NO ACTION');

  RETURN QUERY SELECT fk_ok('cookbook_artifact_version_checksums',
                            ARRAY['org_id', 'checksum'],
                            'checksums',
                            ARRAY['org_id', 'checksum']);

  RETURN QUERY SELECT chef_pgtap.fk_update_action_is('cookbook_artifact_version_checksums_org_id_fkey', 'CASCADE');
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('cookbook_artifact_version_checksums_org_id_fkey', 'RESTRICT');

END;
$$;
