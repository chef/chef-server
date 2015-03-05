CREATE OR REPLACE FUNCTION test_data_bag_items_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN QUERY SELECT has_table('data_bag_items');

  -- Columns

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('data_bag_items', 'id');
  RETURN QUERY SELECT col_is_pk('data_bag_items', 'id');

  RETURN QUERY SELECT chef_pgtap.col_is_uuid('data_bag_items', 'org_id');
  RETURN QUERY SELECT chef_pgtap.col_is_name('data_bag_items', 'data_bag_name');
  RETURN QUERY SELECT chef_pgtap.col_is_name('data_bag_items', 'item_name');
  RETURN QUERY SELECT chef_pgtap.col_is_uuid('data_bag_items', 'last_updated_by');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('data_bag_items', 'created_at');
  RETURN QUERY SELECT chef_pgtap.col_is_timestamp('data_bag_items', 'updated_at');
  RETURN QUERY SELECT chef_pgtap.col_is_blob('data_bag_items', 'serialized_object', TRUE); -- TODO: should be NOT NULL

  RETURN QUERY SELECT col_is_unique('data_bag_items', ARRAY['org_id', 'data_bag_name', 'item_name']);

  -- Indexes

  RETURN QUERY SELECT chef_pgtap.has_index('data_bag_items',
                         'data_bag_items_org_id_data_bag_name_item_name_key',
                         ARRAY['org_id', 'data_bag_name', 'item_name']);

  -- Keys

  RETURN QUERY SELECT has_pk('data_bag_items');
  RETURN QUERY SELECT has_fk('data_bag_items');

  RETURN QUERY SELECT fk_ok('data_bag_items', ARRAY['org_id', 'data_bag_name'],
             'data_bags', ARRAY['org_id', 'name']);

  RETURN QUERY SELECT chef_pgtap.fk_update_action_is('data_bag_items_org_id_fkey', 'CASCADE');
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('data_bag_items_org_id_fkey', 'CASCADE');

END;
$$;
