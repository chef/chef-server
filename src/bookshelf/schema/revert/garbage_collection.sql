-- Revert bookshelf:garbage_collection from pg

BEGIN;

-- XXX Add DDLs here.
DROP TRIGGER delete_file_last_reference_on_delete ON file_names CASCADE;
DROP TRIGGER delete_file_last_reference_on_update ON file_names CASCADE;

DROP FUNCTION IF EXISTS delete_file_last_reference();

COMMIT;
