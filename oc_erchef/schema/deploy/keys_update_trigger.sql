-- Deploy keys_update_trigger
--
-- This keeps the keys table consistent when the clients and user tables update their keys
--
BEGIN;

CREATE OR REPLACE FUNCTION add_key() RETURNS TRIGGER AS $add_key$
   BEGIN
     IF NEW.public_key IS NOT NULL THEN
       INSERT INTO keys
         (id, key_name, public_key, key_version, created_at,  expires_at) VALUES
         (NEW.id, 'default', NEW.public_key, NEW.pubkey_version, now(), 'infinity'::timestamp);
     END IF;
     RETURN NULL; -- result is ignored since this is an AFTER trigger
   END;
$add_key$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION update_key() RETURNS TRIGGER AS $update_key$
   BEGIN
     IF NEW.public_key IS NOT NULL THEN
       UPDATE keys SET public_key = NEW.public_key, key_version = NEW.pubkey_version, expires_at = 'infinity'::timestamp
         WHERE id = NEW.id AND key_name = 'default';
       INSERT INTO  keys (id, key_name, public_key, key_version, created_at, expires_at)
         SELECT NEW.id, 'default', NEW.public_key, NEW.pubkey_version, now(), 'infinity'::timestamp
         WHERE NOT EXISTS (SELECT 1 FROM keys WHERE id = NEW.id AND key_name = 'default');
     ELSE
       DELETE FROM keys WHERE id = NEW.id AND key_name = 'default';
     END IF;
     RETURN NULL; -- result is ignored since this is an AFTER trigger
   END;
$update_key$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS add_key ON clients;
DROP TRIGGER IF EXISTS update_key ON clients;

DROP TRIGGER IF EXISTS add_key ON users;
DROP TRIGGER IF EXISTS update_key ON users;

CREATE TRIGGER add_key AFTER INSERT ON clients FOR EACH ROW EXECUTE PROCEDURE add_key();
CREATE TRIGGER update_key AFTER UPDATE ON clients FOR EACH ROW EXECUTE PROCEDURE update_key();

CREATE TRIGGER add_key AFTER INSERT ON users FOR EACH ROW EXECUTE PROCEDURE add_key();
CREATE TRIGGER update_key AFTER UPDATE ON users FOR EACH ROW EXECUTE PROCEDURE update_key();

COMMIT;
