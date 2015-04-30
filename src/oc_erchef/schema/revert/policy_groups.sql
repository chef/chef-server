-- Revert policy_groups

BEGIN;

ALTER TABLE policies DROP CONSTRAINT policies_name_org_id_unique;

ALTER TABLE policies ADD COLUMN policy_group VARCHAR(256);
ALTER TABLE policies ADD CONSTRAINT policies_name_group_org_id_key UNIQUE(name, policy_group, org_id);

DROP TABLE IF EXISTS policy_groups;

COMMIT;
