-- Revert keys_by_name_add_disabled
-- Remove disabled column from keys_by_name view

BEGIN;

CREATE OR REPLACE view keys_by_name AS
SELECT id, org_id, name, authz_id, 'client' AS type, key_name, keys.public_key, key_version, expires_at FROM clients INNER JOIN keys USING (id)
UNION ALL
SELECT id, 'global', username AS name, authz_id, 'user' AS type, key_name, keys.public_key, key_version, expires_at FROM users INNER JOIN keys USING (id);

COMMIT;
