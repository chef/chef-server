-- Verify debug_schema

BEGIN;

SELECT pg_catalog.has_schema_privilege('debug', 'usage');

ROLLBACK;
