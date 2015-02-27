-- Verify clients

BEGIN;

SELECT id, org_id, authz_id, name, public_key,
       validator, last_updated_by, created_at,
       updated_at, pubkey_version, admin
FROM clients
WHERE FALSE;

ROLLBACK;
