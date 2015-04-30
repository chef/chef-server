-- Revert cookbook_versions_cookbook_id_index

BEGIN;

DROP INDEX IF EXISTS cookbook_versions_cookbook_id_index;

COMMIT;
