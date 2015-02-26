-- Verify new columns added to users

BEGIN;

SELECT hashed_password, salt, hash_type FROM users WHERE FALSE;

ROLLBACK;
