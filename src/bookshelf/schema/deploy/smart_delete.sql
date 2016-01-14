-- Deploy smart_delete

BEGIN;

-- XXX Add DDLs here.

ALTER TABLE file_data ADD COLUMN delete_after timestamp without time zone default NULL;

CREATE INDEX file_data_delete_after_index on file_data(delete_after);

CREATE OR REPLACE FUNCTION mark_deleted(
       id file_data.data_id%TYPE,
       delay interval)
RETURNS file_data.delete_after%TYPE -- what happens if exists already? TODO
AS $$
DECLARE
   expire_time file_data.delete_after%TYPE;
BEGIN
   SELECT (now() at time zone 'utc') + delay INTO expire_time;
   UPDATE file_data SET delete_after = expire_time WHERE data_id = id;
   raise WARNING 'expiring (% %)', id, expire_time;
   RETURN expire_time;
END;
$$
LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION purge_expired()
RETURNS integer
AS $$
DECLARE
   deleted integer;
BEGIN
   WITH a AS (DELETE FROM file_data WHERE delete_after < now() RETURNING 1)
   SELECT count(*) FROM a INTO deleted;
   RETURN deleted;
END;
$$
LANGUAGE plpgsql VOLATILE;

-- this replaces the existing garbage collection
CREATE OR REPLACE FUNCTION delete_file_last_reference() RETURNS TRIGGER as $delete_file_last_reference$
   BEGIN
       raise WARNING 'reference (%, %)', OLD.name, OLD.data_id;
       IF NOT EXISTS (SELECT 1 FROM file_names WHERE data_id = OLD.data_id) THEN
         raise WARNING 'deleting (%)', OLD.data_id;
         PERFORM mark_deleted(OLD.data_id, '30m');
       END IF;
       RETURN NULL; -- result is ignored since this is an AFTER trigger
   END;
$delete_file_last_reference$ LANGUAGE plpgsql;

COMMIT;

-- Find and cleanup orphan filedata structures
-- Base query to list these is:
-- WITH dead_files AS (SELECT file_data.data_id, file_data.complete, file_data.chunk_count
--   FROM file_data LEFT JOIN file_names ON file_data.data_id =
--   file_names.data_id WHERE bucket_id IS NULL )
--   SELECT data_id FROM dead_files;
CREATE OR REPLACE FUNCTION gc_data_ids()
RETURNS integer
AS $$
DECLARE
  deleted integer;
BEGIN
   WITH dead_files AS (SELECT file_data.data_id, file_data.complete, file_data.chunk_count
     FROM file_data LEFT JOIN file_names
     ON file_data.data_id = file_names.data_id
     WHERE bucket_id IS NULL ),
     removed AS (DELETE FROM file_data USING dead_files WHERE file_data.data_id = dead_files.data_id RETURNING 1)
   SELECT count(*) FROM removed INTO deleted;
   RETURN deleted;
END;
$$
LANGUAGE plpgsql VOLATILE;

       
