-- Deploy delete_migrated_couch_data
-- requires: oss_chef:cookbook_version_checksums
-- requires: oss_chef:checksums
-- requires: oss_chef:cookbook_versions
-- requires: oss_chef:cookbooks
-- requires: oss_chef:environments
-- requires: oss_chef:roles
-- requires: oss_chef:clients
-- requires: oss_chef:data_bag_items
-- requires: oss_chef:data_bags

BEGIN;

CREATE OR REPLACE FUNCTION delete_migrated_couch_data(p_org_id checksums.org_id%TYPE)
RETURNS VOID
SECURITY DEFINER AS $$
BEGIN
    DELETE FROM cookbook_version_checksums WHERE org_id = p_org_id;
    DELETE FROM checksums WHERE org_id = p_org_id;
    DELETE FROM cookbook_versions USING cookbooks WHERE cookbook_id = cookbooks.id AND org_id = p_org_id;
    DELETE FROM cookbooks WHERE org_id = p_org_id;
    DELETE FROM environments WHERE org_id = p_org_id;
    DELETE FROM roles WHERE org_id = p_org_id;
    DELETE FROM clients WHERE org_id = p_org_id;
    DELETE FROM data_bag_items WHERE org_id = p_org_id;
    DELETE FROM data_bags WHERE org_id = p_org_id;
    RETURN;
END
$$ LANGUAGE plpgsql;

COMMIT;
