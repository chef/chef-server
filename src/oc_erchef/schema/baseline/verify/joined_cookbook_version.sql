-- Verify joined_cookbook_version

BEGIN;

SELECT major, minor, patch, version, serialized_object, id, org_id, name
FROM joined_cookbook_version
WHERE FALSE;

ROLLBACK;
