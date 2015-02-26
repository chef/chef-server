-- Verify cookbook_artifacts

BEGIN;

SELECT id, org_id, name, authz_id
FROM cookbook_artifacts
WHERE FALSE;

ROLLBACK;
