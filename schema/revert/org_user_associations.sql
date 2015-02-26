-- Revert org_user_associations

BEGIN;

DROP TABLE IF EXISTS org_user_associations;

COMMIT;
