-- Revert debug_schema

BEGIN;

DROP SCHEMA IF EXISTS debug;

COMMIT;
