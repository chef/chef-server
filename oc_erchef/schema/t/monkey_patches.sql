-- Currently, a few of our UUID columns in Enterprise Chef are 36
-- characters wide, instead of 32.  This is to account for the fact
-- that some of the IDs in CouchDB and Solr had hyphens in them.  Once
-- we can go in an fix those, we can reduce the width back down to 32
-- characters across the board (or, better yet, move to actual UUID
-- types).
--
-- In any event, this will replace the testing function we currently
-- have in place.
--
-- See
-- https://github.com/opscode/chef-sql-schema/commit/be4f85a1429e2b02aadea425e6e77fdcfe3d1c71
-- for more details.
CREATE OR REPLACE FUNCTION chef_pgtap.is_wide_uuid(p_table_name NAME, p_column_name NAME)
RETURNS BOOLEAN
LANGUAGE SQL
AS $$
  SELECT (p_table_name = 'data_bag_items' AND p_column_name = 'id') OR
         (p_table_name = 'clients' AND p_column_name = 'id') OR
         (p_table_name = 'data_bags' AND p_column_name = 'id') OR
         (p_table_name = 'roles' AND p_column_name = 'id') OR
         (p_table_name = 'environments' AND p_column_name = 'id') OR
	 (p_table_name = 'containers' AND p_column_name = 'id') OR
	 (p_table_name = 'groups' AND p_column_name = 'id') OR
	 (p_table_name = 'organizations' AND p_column_name = 'couchdb_id') OR
	 (p_table_name = 'org_user_associations' AND p_column_name = 'user_id') OR
	 (p_table_name = 'org_user_invites' AND p_column_name = 'user_id')

$$;

CREATE OR REPLACE FUNCTION chef_pgtap.col_is_uuid(p_table_name NAME, p_column_name NAME, p_is_unique BOOLEAN)
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
   RETURN NEXT has_column(p_table_name, p_column_name);
   RETURN NEXT col_not_null(p_table_name, p_column_name);
   RETURN NEXT col_type_is(p_table_name,
                           p_column_name,
                           CASE WHEN chef_pgtap.is_wide_uuid(p_table_name, p_column_name)
                                THEN 'character varying(36)'
                                ELSE 'character(32)'
                           END);
   RETURN NEXT col_hasnt_default(p_table_name, p_column_name);
   IF p_is_unique THEN
     RETURN NEXT col_is_unique(p_table_name, p_column_name);
     -- TODO: it'd be nice to have a col_isnt_unique function...
   END IF;
END;
$$;
