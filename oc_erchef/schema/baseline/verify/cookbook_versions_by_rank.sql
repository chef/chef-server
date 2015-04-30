-- Verify cookbook_versions_by_rank

BEGIN;

SELECT major, minor, patch, version,
       serialized_object, org_id, name,
       rank
FROM cookbook_versions_by_rank
WHERE FALSE;

ROLLBACK;
