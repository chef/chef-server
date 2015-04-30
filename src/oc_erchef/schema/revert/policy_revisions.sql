-- Revert policy_revisions

BEGIN;

DROP TABLE IF EXISTS policy_revisions;

ALTER TABLE policies ADD COLUMN serialized_object bytea;
ALTER TABLE policies DROP CONSTRAINT policies_org_id_authz_id_name_unique;

COMMIT;
