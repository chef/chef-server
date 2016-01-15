-- Deploy bookshelf:utility_functions to pg

BEGIN;

-- XXX Add DDLs here.
-- Insert file function to ease sqerl interface
CREATE OR REPLACE FUNCTION create_file_by_bucket_id(
       bucket_id file_names.bucket_id%TYPE,
       new_name   file_names.name%TYPE )
RETURNS file_data.data_id%TYPE -- what happens if exists already? TODO
AS $$
DECLARE
   new_id file_data.data_id%TYPE;
BEGIN
   INSERT INTO file_data (complete) VALUES ('false') RETURNING data_id INTO new_id;
   INSERT INTO file_names (bucket_id, "name", data_id) VALUES (bucket_id, new_name, new_id);
   RETURN new_id;
END;
$$
LANGUAGE plpgsql VOLATILE;

-- DEPRECATED?
CREATE OR REPLACE FUNCTION create_file_by_bucket_name(
       bn bucket_names.bucket_name%TYPE,
       new_name  file_names.name%TYPE )
RETURNS file_data.data_id%TYPE -- what happens if exists already? TODO
AS $$
DECLARE
   bucket_id file_names.bucket_id%TYPE;
   new_data_id file_data.data_id%TYPE;
BEGIN
   SELECT b.bucket_id INTO bucket_id FROM bucket_names AS b WHERE b.bucket_name = bn;
   INSERT INTO file_data (complete) VALUES ('false') RETURNING data_id INTO new_data_id;
   INSERT INTO file_names (bucket_id, "name", data_id) VALUES (bucket_id, new_name, new_data_id);
   RETURN new_data_id;
END;
$$
LANGUAGE plpgsql VOLATILE;

-- Replace a file; used as the final phase of upload
CREATE OR REPLACE FUNCTION rename_file_with_overwrite(
       target_bucket_id file_names.bucket_id%TYPE,
       target_file_id file_names.file_id%TYPE,
       new_name file_names.name%TYPE)
RETURNS boolean
AS $$
BEGIN
   --  Don't delete yourself if you are no-op ing
   DELETE FROM file_names WHERE bucket_id = target_bucket_id AND name = new_name AND file_id != target_file_id;
   UPDATE file_names SET name = new_name WHERE file_id = target_file_id;
   RETURN true;
END;
$$
LANGUAGE plpgsql VOLATILE;

-- DEPRECATED?
CREATE OR REPLACE FUNCTION replace_chunk_data(
       target_file_id file_names.file_id%TYPE )
RETURNS file_data.data_id%TYPE -- what happens if exists already? TODO
AS $$
DECLARE
   new_data_id file_data.data_id%TYPE;
BEGIN
   INSERT INTO file_data (complete) VALUES ('false') returning data_id INTO new_data_id;
   UPDATE file_names fn SET data_id = new_data_id WHERE fn.file_id = target_file_id;
   RETURN new_data_id;
END;
$$
LANGUAGE plpgsql VOLATILE;

COMMIT;
