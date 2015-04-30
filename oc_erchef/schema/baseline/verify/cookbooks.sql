-- Verify cookbooks

BEGIN;

SELECT id, org_id, name, authz_id
FROM cookbooks
WHERE FALSE;

ROLLBACK;
