-- add new columns for password hash/salt/salt type to users table
-- requires: users
-- requires: password_hash_type

BEGIN;

ALTER TABLE users
  ADD COLUMN hashed_password TEXT,
  ADD COLUMN salt TEXT,
  ADD COLUMN hash_type password_hash_type,
  ADD CONSTRAINT password_hash_deps CHECK (
    (hashed_password IS NULL AND salt IS NULL and hash_type IS NULL) OR
    (hashed_password IS NOT NULL AND salt IS NOT NULL and hash_type IS NOT NULL)
  );

COMMIT;
