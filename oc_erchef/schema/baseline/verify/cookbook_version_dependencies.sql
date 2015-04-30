-- Verify cookbook_version_dependencies

BEGIN;

SELECT org_id, name, major, minor, patch, dependencies
FROM cookbook_version_dependencies
WHERE FALSE;

ROLLBACK;
