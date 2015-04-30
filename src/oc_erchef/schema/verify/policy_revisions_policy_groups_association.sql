-- Verify policy_revisions_policy_groups_association

BEGIN;

SELECT id, org_id, policy_revision_revision_id, policy_revision_name, policy_group_name, last_updated_by
FROM policy_revisions_policy_groups_association
WHERE FALSE;

ROLLBACK;
