-- Verify reporting_schema_info

BEGIN;

SELECT version
FROM reporting_schema_info
WHERE FALSE;

ROLLBACK;
