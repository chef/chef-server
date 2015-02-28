-- Verify policy_revisions

BEGIN;

SELECT id, org_id, revision_id, name, last_updated_by, serialized_object
FROM policy_revisions
WHERE FALSE;

ROLLBACK;
