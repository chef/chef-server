-- Verify users

BEGIN;

SELECT id, authz_id, username, email, pubkey_version,
       public_key, serialized_object, last_updated_by,
       created_at, updated_at, external_authentication_uid,
       recovery_authentication_enabled, admin
FROM users
WHERE FALSE;

ROLLBACK;
