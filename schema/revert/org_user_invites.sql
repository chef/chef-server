-- Revert org_user_invites

BEGIN;

DROP TABLE IF EXISTS org_user_invites;

COMMIT;
