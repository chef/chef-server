-- Revert joined_cookbook_version

BEGIN;

DROP VIEW IF EXISTS joined_cookbook_version;

COMMIT;
