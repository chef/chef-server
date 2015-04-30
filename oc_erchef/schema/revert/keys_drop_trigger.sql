-- Revert keys_drop_trigger

BEGIN;

DROP TRIGGER IF EXISTS delete_keys ON clients;
DROP TRIGGER IF EXISTS delete_keys ON users;
DROP FUNCTION IF EXISTS delete_keys();

COMMIT;
