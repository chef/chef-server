-- Revert enterprise_chef:create_and_update_users from pg

BEGIN;

DROP FUNCTION IF EXISTS add_user();
DROP FUNCTION IF EXISTS update_user();

COMMIT;
