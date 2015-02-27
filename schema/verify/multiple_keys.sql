-- Verify multiple_keys

BEGIN;


SELECT id, key_name, public_key, key_version, created_at, expires_at
FROM keys
WHERE FALSE;

ROLLBACK;
