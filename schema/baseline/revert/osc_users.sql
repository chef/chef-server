-- Revert osc_users

BEGIN;

DROP TABLE IF EXISTS osc_users;

COMMIT;
