-- Deploy smart_delete

BEGIN;

CREATE OR REPLACE FUNCTION mark_deleted(id file_data.data_id%TYPE)
RETURNS file_data.tombstoned_at%TYPE
AS $$
DECLARE
   my_tombstone file_data.tombstoned_at%TYPE;
BEGIN
   SELECT (now() at time zone 'utc') INTO my_tombstone;
   UPDATE file_data SET tombstoned_at = my_tombstone WHERE data_id = id AND tombstoned_at IS NULL;
   RETURN my_tombstone;
END;
$$
LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION delete_file_last_reference() RETURNS TRIGGER as $delete_file_last_reference$
   BEGIN
       IF NOT EXISTS (SELECT 1 FROM files WHERE data_id = OLD.data_id) THEN
         PERFORM mark_deleted(OLD.data_id);
       END IF;
       RETURN NULL;
   END;
$delete_file_last_reference$ LANGUAGE plpgsql;

CREATE TRIGGER delete_file_last_reference_on_delete AFTER DELETE ON files FOR EACH ROW EXECUTE PROCEDURE delete_file_last_reference();
CREATE TRIGGER delete_file_last_reference_on_update AFTER UPDATE ON files FOR EACH ROW EXECUTE PROCEDURE delete_file_last_reference();

-- These functions are called by bookshelf regularly
CREATE OR REPLACE FUNCTION purge_expired(delay integer)
RETURNS integer
AS $$
DECLARE
   expire_time file_data.tombstoned_at%TYPE;
   deleted integer;
BEGIN
   SELECT (now() at time zone 'utc') - (delay * '1 millisecond'::interval) INTO expire_time;
   WITH a AS (DELETE FROM file_data WHERE tombstoned_at < expire_time RETURNING 1)
   SELECT count(*) FROM a INTO deleted;
   RETURN deleted;
END;
$$
LANGUAGE plpgsql VOLATILE;


-- Abandoned uploads are uploads which have not been completed
-- within some user-configurable time period.

-- Note that the completed field is updated in the same transation
-- which links the file_data to the files, so any referenced
-- file_data will have completed = true.
CREATE OR REPLACE FUNCTION cleanup_abandoned_uploads(delay integer)
RETURNS integer
AS $$
DECLARE
  expire_time file_data.upload_started_at%TYPE;
  deleted integer;
BEGIN
   SELECT (now() at time zone 'utc') - (delay * '1 millisecond'::interval) INTO expire_time;
   WITH removed AS
     (DELETE FROM file_data WHERE complete = false AND upload_started_at < expire_time RETURNING 1)
   SELECT count(*) FROM removed INTO deleted;
   RETURN deleted;
END;
$$
LANGUAGE plpgsql VOLATILE;

COMMIT;
