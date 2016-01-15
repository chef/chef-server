-- Deploy bookshelf:base_schema to pg

BEGIN;

CREATE TABLE IF NOT EXISTS bucket_names(
   bucket_name text NOT NULL PRIMARY KEY,
   created_at timestamp without time zone default (now() at time zone 'utc'),
   bucket_id serial NOT NULL
);

CREATE UNIQUE INDEX bucket_names_bucket_id_index on bucket_names(bucket_id);

-- By convention we can infer the org id from the name used. Some
-- maintenance operations would be much faster if we could index on
-- that. For now, we will want to make sure we can index on a prefix
-- efficiently.
CREATE TABLE IF NOT EXISTS file_names(
    file_id    serial NOT NULL,
    bucket_id  int NOT NULL,
    name       text NOT NULL,
    created_at timestamp without time zone default (now() at time zone 'utc'),
    CONSTRAINT file_names_bucket_name_key UNIQUE(bucket_id, name),
    data_id    bigint NOT NULL
);

CREATE UNIQUE INDEX file_names_file_id_index ON file_names(file_id);

--
-- This is separate from the file table because that is apparently a
-- good pattern if we use blobs with OIDs, and it also allows
-- deduplication.
--

-- Encoding choices for hashes.

CREATE TABLE IF NOT EXISTS file_data(
    data_id           bigserial PRIMARY KEY,
    upload_started_at timestamp without time zone default (now() at time zone 'utc'),
    complete          boolean,
    data_size         bigint, 
    chunk_count       int,

    -- Normal practice would be to constrain hash_* fields to be NOT
    -- NULL UNIQUE, but if we are streaming the file we won't know
    -- those until the end, so they will be NULL at first.
    -- Instead of starting with NULL, we could use a dummy hash and change it
    -- after the fact. Also, there exist known collisions for md5;
    -- relying on it being unique is unwise.

    -- We're storing hash as raw bytes for efficiency, with the assumption that bytea indexing will work well
    hash_md5    bytea, -- 160 bits as binary (20B)

    -- Might want to store sha256 as well/instead, because the S3 v4 api has a field for that.
    hash_sha256 bytea, -- 256 bits as binary (32B)
    
    -- This exists to allow deduplication. sha512 is faster than
    -- sha256, and 32 extra bytes per file seems pretty low impact.
    -- 256 bits would be ample for simple collision by accident, but
    -- 512 offers resistance against dedicated attack.
    -- rule of thumb for dedup is prob of collision is p = number_of_files^2/(2*2^bits) or
    hash_sha512 bytea -- 512 bits as binary (64B)
);

CREATE INDEX file_data_hash_md5_index ON file_data(hash_md5);
CREATE INDEX file_data_hash_sha512_index ON file_data(hash_sha512);

-- We would like to do reference counting for the file_data entries so that when the last referring
-- file_name entry goes away so does it. However this merely makes sure that every file_name references a valid
-- file data item.
ALTER TABLE file_names ADD CONSTRAINT file_names_file_id_fk FOREIGN KEY (data_id) REFERENCES file_data(data_id) ON DELETE RESTRICT;

-- Initial design uses chunks of bytea, to allow for chunked streaming of data instead of one giant put.
-- A (possibly) better design would use blobs
-- 
-- Storage of data as chunks avoids 1GB limit on bytea structures, allows more efficient streaming
--
CREATE TABLE IF NOT EXISTS file_chunks(
    data_id     bigint,
    chunk       integer,
    CONSTRAINT file_chunks_data_id_chunk_key UNIQUE(data_id, chunk), 
    data        bytea
);

ALTER TABLE file_chunks ADD CONSTRAINT file_chunks_id_fk FOREIGN KEY (data_id) REFERENCES file_data(data_id) ON DELETE CASCADE ;

CREATE OR REPLACE VIEW expanded_files AS SELECT bucket_name, fd.* FROM bucket_names b INNER JOIN
       (SELECT bucket_id, name, created_at, file_id, d.* FROM
               file_names f INNER JOIN file_data d ON f.data_id = d.data_id) fd
	ON b.bucket_id = fd.bucket_id;	

--
-- This atomically links a file data item with 
-- 
CREATE OR REPLACE FUNCTION link_file_data (
       my_file_id file_names.file_id%TYPE,
       my_data_id file_data.data_id%TYPE
       )
       RETURNS VOID -- fix this to be null
AS $$
BEGIN
  UPDATE file_data SET complete = true WHERE data_id = my_data_id;
  UPDATE file_names SET data_id = my_data_id WHERE file_id = my_file_id;
END
$$
LANGUAGE plpgsql VOLATILE;


--
-- This could be refactored to use link_file_data above, but it seems
-- simpler to do it this way.
--
CREATE OR REPLACE FUNCTION create_file_link_data(
       bn bucket_names.bucket_name%TYPE,
       my_name  file_names.name%TYPE,
       my_data_id file_data.data_id%TYPE)
RETURNS file_names.file_id%TYPE -- what happens if exists already? UPSERT?
AS $$
DECLARE
   my_bucket_id file_names.bucket_id%TYPE;
   new_file_id file_names.file_id%TYPE;
BEGIN
   SELECT b.bucket_id INTO my_bucket_id FROM bucket_names AS b WHERE b.bucket_name = bn;
   UPDATE file_data SET complete = true WHERE data_id = my_data_id;
   INSERT INTO file_names (bucket_id, "name", data_id) VALUES (my_bucket_id, my_name, my_data_id)
   	       RETURNING file_id INTO new_file_id;
   RETURN new_file_id;	       
END;
$$
LANGUAGE plpgsql VOLATILE;       

COMMIT;
