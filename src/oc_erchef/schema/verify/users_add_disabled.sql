-- Verify users_add_disabled

BEGIN;

SELECT disabled FROM users WHERE FALSE;

ROLLBACK;
