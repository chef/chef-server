-- Verify kvpair

BEGIN;

  SELECT (1/COUNT(*)) AS result FROM kvpair WHERE key = 'itime';

ROLLBACK;
