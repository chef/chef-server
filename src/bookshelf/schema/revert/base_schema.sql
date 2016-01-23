-- Revert bookshelf:base_schema from pg
BEGIN;

DROP FUNCTION IF EXISTS link_file_data(my_file_id files.id%TYPE, my_data_id file_data.data_id%TYPE);
DROP FUNCTION IF EXISTS create_file_link_data(bn buckets.name%TYPE, my_name files.name%TYPE, my_data_id file_data.data_id%TYPE);

ALTER TABLE files DROP CONSTRAINT files_data_id_fk;
ALTER TABLE file_chunks DROP CONSTRAINT file_chunks_id_fk;

DROP INDEX IF EXISTS buckets_id_index;

DROP INDEX IF EXISTS files_id_index;

DROP INDEX IF EXISTS file_data_hash_md5_index;
DROP INDEX IF EXISTS file_data_hash_sha512_index;
DROP INDEX IF EXISTS file_data_tombstoned_at_index;
DROP INDEX IF EXISTS file_data_upload_started_at_index;


DROP VIEW IF EXISTS expanded_files;

DROP TABLE IF EXISTS buckets CASCADE;
DROP TABLE IF EXISTS files CASCADE;
DROP TABLE IF EXISTS file_data CASCADE;
DROP TABLE IF EXISTS file_chunks CASCADE;

COMMIT;
