BEGIN;

ALTER TABLE users
  DROP CONSTRAINT password_hash_deps,
  DROP COLUMN hashed_password,
  DROP COLUMN salt,
  DROP COLUMN hash_type;

COMMIT;

