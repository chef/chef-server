BEGIN;

SELECT 1/COUNT(*) FROM pg_type WHERE typname = 'password_hash_type';

ROLLBACK;
