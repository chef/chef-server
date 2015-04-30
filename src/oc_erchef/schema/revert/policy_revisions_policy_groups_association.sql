-- Revert policy_revisions_policy_groups_association

BEGIN;

DROP TABLE IF EXISTS policy_revisions_policy_groups_association;

COMMIT;
