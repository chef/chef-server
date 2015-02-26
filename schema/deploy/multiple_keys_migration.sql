-- Deploy multiple_keys_migration
--
-- Populate keys table from users and clients table.
BEGIN;
--
-- Initial migration
-- The 'WHERE NOT EXISTS' clauses should prevent inserts when we already have a key.
--


CREATE OR REPLACE FUNCTION migrate_keys() RETURNS void AS $migrate_keys$
  BEGIN
    -- This used to be wrapped in a lock 'LOCK TABLE keys IN SHARE ROW EXCLUSIVE MODE' but that
    -- is redundant with the function execution.
    -- add clients
    INSERT INTO "keys"
      (id, key_name, public_key, key_version, created_at,  expires_at)
	SELECT id, 'default', public_key, pubkey_version, updated_at, 'infinity'::timestamp FROM clients
      WHERE NOT EXISTS (SELECT 1 FROM keys where id=clients.id);
    -- add users
    INSERT INTO "keys"
      (id, key_name, public_key, key_version, created_at,  expires_at)
	SELECT id, 'default', public_key, pubkey_version, updated_at, 'infinity'::timestamp FROM users
      WHERE NOT EXISTS (SELECT 1 FROM keys where id=users.id);
  END;
$migrate_keys$ LANGUAGE plpgsql;
COMMIT;
