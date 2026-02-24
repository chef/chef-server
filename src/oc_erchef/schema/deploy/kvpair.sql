-- Deploy kvpair

BEGIN;

  CREATE TABLE IF NOT EXISTS kvpair (
    key text NOT NULL PRIMARY KEY,
    value text NOT NULL,
    created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
  );

  -- Capture record of migration run timestamp
  INSERT INTO kvpair (key, value, created_at)
    VALUES ('itime', (SELECT EXTRACT(EPOCH FROM CURRENT_TIMESTAMP)::bigint::text), CURRENT_TIMESTAMP)
    ON CONFLICT(key) DO NOTHING;

COMMIT;
