-- Deploy bookshelf:base_schema to pg

BEGIN;

CREATE TABLE IF NOT EXISTS buckets(
   name text NOT NULL PRIMARY KEY,
   created_at timestamp without time zone default (now() at time zone 'utc'),
   id serial NOT NULL
);

CREATE UNIQUE INDEX buckets_id_index on buckets(id);

-- By convention we can infer the org id from the name used. Some
-- maintenance operations would be much faster if we could index on
-- that. For now, we will want to make sure we can index on a prefix
-- efficiently.
CREATE TABLE IF NOT EXISTS files(
    id    serial NOT NULL,
    bucket_id  int NOT NULL,
    name       text NOT NULL,
    created_at timestamp without time zone default (now() at time zone 'utc'),
    CONSTRAINT files_bucket_name_key UNIQUE(bucket_id, name),
    data_id    bigint NOT NULL
);

CREATE UNIQUE INDEX files_id_index ON files(id);

--
-- This is separate from the file table because that is apparently a
-- good pattern if we use blobs with OIDs, and it also allows
-- deduplication.
--

-- Encoding choices for hashes.

CREATE TABLE IF NOT EXISTS file_data(
    data_id           bigserial PRIMARY KEY,
    upload_started_at timestamp without time zone default (now() at time zone 'utc'),
    tombstoned_at     timestamp without time zone default NULL,
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
CREATE INDEX file_data_tombstoned_at_index on file_data(tombstoned_at);
CREATE INDEX file_data_upload_started_at_index on file_data(upload_started_at);

-- We would like to do reference counting for the file_data entries so that when the last referring
-- files entry goes away so does it. However this merely makes sure that every files references a valid
-- file data item.
ALTER TABLE files ADD CONSTRAINT files_data_id_fk FOREIGN KEY (data_id) REFERENCES file_data(data_id) ON DELETE RESTRICT;

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

CREATE OR REPLACE VIEW expanded_files AS SELECT b.name AS bucket_name, fd.*
     FROM buckets b
     INNER JOIN
       (SELECT bucket_id, name, created_at, id AS file_id, d.* FROM
               files f INNER JOIN file_data d ON f.data_id = d.data_id) fd
	ON b.id = fd.bucket_id;

--
-- This atomically updates a file_name item to point at a newly
-- uploaded file_data item. Used on object updates which are rare in
-- our use of bookshelf.
--
CREATE OR REPLACE FUNCTION link_file_data (
       my_file_id files.id%TYPE,
       my_data_id file_data.data_id%TYPE
       )
       RETURNS VOID -- fix this to be null
AS $$
BEGIN
  UPDATE file_data SET complete = true WHERE data_id = my_data_id;
  UPDATE files SET data_id = my_data_id WHERE id = my_file_id;
END
$$
LANGUAGE plpgsql VOLATILE;

--
-- This could be refactored to use link_file_data above, but it seems
-- simpler to do it this way.
--
CREATE OR REPLACE FUNCTION create_file_link_data(
       bn buckets.name%TYPE,
       my_name  files.name%TYPE,
       my_data_id file_data.data_id%TYPE)
RETURNS files.id%TYPE -- what happens if exists already? UPSERT?
AS $$
DECLARE
   my_bucket_id buckets.id%TYPE;
   new_file_id files.id%TYPE;
BEGIN
   SELECT b.id INTO my_bucket_id FROM buckets AS b WHERE b.name = bn;
   UPDATE file_data SET complete = true WHERE data_id = my_data_id;
   INSERT INTO files (bucket_id, "name", data_id) VALUES (my_bucket_id, my_name, my_data_id)
   	       RETURNING id INTO new_file_id;
   RETURN new_file_id;
END;
$$
LANGUAGE plpgsql VOLATILE;

COMMIT;
