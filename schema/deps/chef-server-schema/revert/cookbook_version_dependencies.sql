-- Revert cookbook_version_dependencies

BEGIN;

DROP VIEW IF EXISTS cookbook_version_dependencies;

COMMIT;
