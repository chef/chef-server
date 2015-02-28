-- Revert policy_revisions

BEGIN;

DROP TABLE IF EXISTS policy_revisions;

ALTER TABLE policies ADD COLUMN serialized_object bytea;

COMMIT;
