CREATE OR REPLACE FUNCTION test_cookbook_versions_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('cookbook_versions');

  -- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('cookbook_versions', 'id');
  RETURN QUERY SELECT col_is_pk('cookbook_versions', 'id');

  RETURN QUERY SELECT chef_pgtap.col_is_version_component('cookbook_versions', 'major');
  RETURN QUERY SELECT chef_pgtap.col_is_version_component('cookbook_versions', 'minor');
  RETURN QUERY SELECT chef_pgtap.col_is_version_component('cookbook_versions', 'patch');

  -- TODO: need to default this to FALSE so we can use chef_pgtap.col_is_flag
  RETURN QUERY SELECT has_column('cookbook_versions', 'frozen');
  RETURN QUERY SELECT col_not_null('cookbook_versions', 'frozen');
  RETURN QUERY SELECT col_type_is('cookbook_versions', 'frozen', 'boolean');
  RETURN QUERY SELECT col_hasnt_default('cookbook_versions', 'frozen');

  RETURN QUERY SELECT chef_pgtap.col_is_blob('cookbook_versions', 'meta_attributes');

  RETURN QUERY SELECT has_column('cookbook_versions', 'meta_deps');
  RETURN QUERY SELECT col_not_null('cookbook_versions', 'meta_deps');
  RETURN QUERY SELECT col_type_is('cookbook_versions', 'meta_deps', 'text');
  RETURN QUERY SELECT col_hasnt_default('cookbook_versions', 'meta_deps');
  RETURN QUERY SELECT chef_pgtap.col_is_storage_type('cookbook_versions', 'meta_deps', 'EXTENDED');

  RETURN QUERY SELECT chef_pgtap.col_is_blob('cookbook_versions', 'meta_long_desc');
  RETURN QUERY SELECT chef_pgtap.col_is_blob('cookbook_versions', 'metadata');
  RETURN QUERY SELECT chef_pgtap.col_is_blob('cookbook_versions', 'serialized_object');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('cookbook_versions', 'updated_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('cookbook_versions', 'created_at');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('cookbook_versions', 'last_updated_by');

  RETURN QUERY SELECT has_column('cookbook_versions', 'cookbook_id');
  RETURN QUERY SELECT col_not_null('cookbook_versions', 'cookbook_id');
  RETURN QUERY SELECT col_type_is('cookbook_versions', 'cookbook_id', 'integer');
  RETURN QUERY SELECT col_hasnt_default('cookbook_versions', 'cookbook_id');

  RETURN QUERY SELECT col_is_unique('cookbook_versions',
                                     ARRAY['cookbook_id', 'major', 'minor', 'patch']);
  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('cookbook_versions',
                           'cookbook_versions_cookbook_id_major_minor_patch_key',
                           ARRAY['cookbook_id', 'major', 'minor', 'patch']);

  RETURN QUERY SELECT chef_pgtap.has_index('cookbook_versions', 'cookbook_versions_cookbook_id_index', 'cookbook_id');

  -- Keys

  RETURN QUERY SELECT has_pk('cookbook_versions');
  RETURN QUERY SELECT has_fk('cookbook_versions');

  RETURN QUERY SELECT fk_ok('cookbook_versions', 'cookbook_id',
                            'cookbooks', 'id');
  RETURN QUERY SELECT chef_pgtap.fk_update_action_is('cookbook_versions_cookbook_id_fkey', 'NO ACTION'  );
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('cookbook_versions_cookbook_id_fkey', 'RESTRICT')  ;

END;
$$;
