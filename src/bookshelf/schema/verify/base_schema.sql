-- Verify bookshelf:base_schema on pg

BEGIN;

-- Verify all the tables (and expected columns) are present.

-- SELECT pg_catalog.has_schema_privilege('bookshelf', 'usage');


ROLLBACK;
