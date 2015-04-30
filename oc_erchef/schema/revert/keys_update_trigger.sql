-- Revert keys_update_trigger

BEGIN;

DROP TRIGGER IF EXISTS add_key ON clients;
DROP TRIGGER IF EXISTS update_key ON clients;

DROP TRIGGER IF EXISTS add_key ON users;
DROP TRIGGER IF EXISTS update_key ON users;

DROP FUNCTION IF EXISTS add_key();
DROP FUNCTION IF EXISTS update_key();

COMMIT;
