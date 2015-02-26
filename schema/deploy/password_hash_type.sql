BEGIN;

CREATE TYPE password_hash_type AS ENUM ('SHA1',
                                        'SHA1-bcrypt',
                                        'bcrypt');

COMMIT;
