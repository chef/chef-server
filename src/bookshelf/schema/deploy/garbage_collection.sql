-- Deploy bookshelf:garbage_collection to pg

BEGIN;

-- XXX Add DDLs here.
-- Reference counting happens here:
CREATE OR REPLACE FUNCTION delete_file_last_reference() RETURNS TRIGGER as $delete_file_last_reference$
   BEGIN
       raise WARNING 'reference (%)', OLD.data_id;
       IF NOT EXISTS (SELECT 1 FROM file_names WHERE data_id = OLD.data_id) THEN
          raise WARNING 'reference (%)', OLD.data_id;
          DELETE FROM file_data where data_id = OLD.data_id;
       END IF;
       RETURN NULL; -- result is ignored since this is an AFTER trigger
   END;
$delete_file_last_reference$ LANGUAGE plpgsql;

CREATE TRIGGER delete_file_last_reference_on_delete AFTER DELETE ON file_names FOR EACH ROW EXECUTE PROCEDURE delete_file_last_reference();
CREATE TRIGGER delete_file_last_reference_on_update AFTER UPDATE ON file_names FOR EACH ROW EXECUTE PROCEDURE delete_file_last_reference();

COMMIT;
