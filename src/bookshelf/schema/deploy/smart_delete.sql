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
   RETURN expire_time;
END;
$$
LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION purge_expired()
RETURNS integer;
BEGIN
   RETURN DELETE FROM file_data WHERE delete_after < now();
END;
$$
LANGUAGE plpgsql VOLATILE;

COMMIT;


       
