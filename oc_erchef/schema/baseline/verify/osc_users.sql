-- Verify osc_users

BEGIN;

SELECT id, authz_id, username, email,
       public_key, hashed_password, salt, hash_type,
       last_updated_by, created_at, updated_at,
       external_authentication_uid, recovery_authentication_enabled,
       admin
FROM osc_users
WHERE FALSE;

ROLLBACK;
