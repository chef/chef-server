-- Deploy cbv_type

BEGIN;
-- PostgreSQL doesn't support a CREATE TYPE IF NOT EXISTS statement
-- thus we drop and then add inside the transaction.
DROP TYPE IF EXISTS cbv;
CREATE TYPE cbv AS (
       name text,
       major bigint,
       minor bigint,
       patch bigint
);
COMMIT;
