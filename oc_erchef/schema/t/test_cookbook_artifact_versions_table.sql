CREATE OR REPLACE FUNCTION test_cookbook_artifact_versions_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('cookbook_artifact_versions');

  -- Columns
  RETURN QUERY SELECT columns_are('cookbook_artifact_versions', ARRAY['id',
                                                                      'identifier',
                                                                      'metadata',
                                                                      'serialized_object',
                                                                      'created_at',
                                                                      'created_by',
                                                                      'cookbook_artifact_id']);

  RETURN QUERY SELECT col_is_pk('cookbook_artifact_versions', 'id');

  RETURN QUERY SELECT chef_pgtap.col_is_blob('cookbook_artifact_versions', 'metadata');
  RETURN QUERY SELECT chef_pgtap.col_is_blob('cookbook_artifact_versions', 'serialized_object');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('cookbook_artifact_versions', 'created_at');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('cookbook_artifact_versions', 'created_by');

  RETURN QUERY SELECT col_not_null('cookbook_artifact_versions', 'cookbook_artifact_id');
  RETURN QUERY SELECT col_type_is('cookbook_artifact_versions', 'cookbook_artifact_id', 'integer');
  RETURN QUERY SELECT col_hasnt_default('cookbook_artifact_versions', 'cookbook_artifact_id');


  -- Indexes
  RETURN QUERY SELECT col_is_unique('cookbook_artifact_versions',
                                     ARRAY['cookbook_artifact_id', 'identifier']);

  -- Keys
  RETURN QUERY SELECT has_pk('cookbook_artifact_versions');
  RETURN QUERY SELECT has_fk('cookbook_artifact_versions');

  RETURN QUERY SELECT fk_ok('cookbook_artifact_versions', 'cookbook_artifact_id',
                            'cookbook_artifacts', 'id');
  RETURN QUERY SELECT chef_pgtap.fk_update_action_is('cookbook_artifact_versions_cookbook_artifact_id_fkey', 'NO ACTION');
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('cookbook_artifact_versions_cookbook_artifact_id_fkey', 'RESTRICT');

END;
$$;
