-- Deploy osc_password_hash_type_update
-- requires: password_hash_type
-- requires: users_password_hash_split

BEGIN;

ALTER TYPE password_hash_type RENAME TO old_password_hash_type;
CREATE TYPE password_hash_type AS ENUM ('SHA1',
                                        'SHA1-bcrypt',
                                        'bcrypt',
                                        'erlang-bcrypt-0.5.0',
                                        'sha1+bcrypt');

ALTER TABLE IF EXISTS users
  ALTER COLUMN hash_type
  TYPE password_hash_type
  USING hash_type::text::password_hash_type;

DROP TYPE IF EXISTS old_password_hash_type;

COMMIT;
