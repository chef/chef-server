-- Revert osc_password_hash_type_update

BEGIN;

ALTER TYPE password_hash_type RENAME TO old_password_hash_type;
CREATE TYPE password_hash_type AS ENUM ('SHA1',
                                        'SHA1-bcrypt',
                                        'bcrypt'
                                        );

ALTER TABLE IF EXISTS users
  ALTER COLUMN hash_type
  TYPE password_hash_type
  USING hash_type::text::password_hash_type;

DROP TYPE IF EXISTS old_password_hash_type;

COMMIT;
