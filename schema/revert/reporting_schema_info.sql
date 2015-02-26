-- Revert reporting_schema_info

BEGIN;

DROP TABLE IF EXISTS reporting_schema_info;

COMMIT;
