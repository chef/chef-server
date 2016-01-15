-- Revert bookshelf:utility_functions from pg

BEGIN;

DROP FUNCTION IF EXISTS create_file_by_bucket_id(file_names.bucket_id%TYPE, file_names.name%TYPE);

DROP FUNCTION IF EXISTS create_file_by_bucket_name(bucket_names.bucket_name%TYPE, file_names.name%TYPE);

-- XXX Add DDLs here.

COMMIT;
