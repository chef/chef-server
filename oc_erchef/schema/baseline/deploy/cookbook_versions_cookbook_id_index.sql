-- Deploy cookbook_versions_cookbook_id_index
-- requires: cookbook_versions

BEGIN;

CREATE INDEX cookbook_versions_cookbook_id_index on cookbook_versions(cookbook_id);

COMMIT;
