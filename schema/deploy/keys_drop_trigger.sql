-- Deploy keys_drop_trigger
--
-- This maintains consistency of the keys table when clients or users get deleted.
--
BEGIN;

CREATE OR REPLACE FUNCTION delete_keys() RETURNS TRIGGER AS $delete_keys$
  BEGIN
     DELETE FROM keys WHERE id = OLD.id;
     RETURN NULL;
  END;
$delete_keys$ LANGUAGE plpgsql;

-- We drop the trigger to keep things idempotent
DROP TRIGGER IF EXISTS delete_keys ON clients;
DROP TRIGGER IF EXISTS delete_keys ON users;

CREATE TRIGGER delete_keys AFTER DELETE ON clients FOR EACH ROW EXECUTE PROCEDURE delete_keys();
CREATE TRIGGER delete_keys AFTER DELETE ON users FOR EACH ROW EXECUTE PROCEDURE delete_keys();

COMMIT;
