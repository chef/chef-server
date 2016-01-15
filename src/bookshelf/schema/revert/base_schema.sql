-- Revert bookshelf:base_schema from pg

BEGIN;

ALTER TABLE file_names DROP CONSTRAINT file_names_file_id_fk;
ALTER TABLE file_chunks DROP CONSTRAINT file_chunks_id_fk;

DROP INDEX bucket_names_bucket_id_index;

DROP INDEX file_names_file_id_index;

DROP INDEX file_data_hash_md5_index;
DROP INDEX file_data_hash_sha512_index;

DROP INDEX file_chunks_id_chunk_index;

DROP VIEW IF EXISTS expanded_files;

DROP TABLE IF EXISTS bucket_names CASCADE;
DROP TABLE IF EXISTS file_names CASCADE;
DROP TABLE IF EXISTS file_data CASCADE;
DROP TABLE IF EXISTS file_chunks CASCADE;

DROP FUNCTION IF EXISTS link_file_data(my_file_id file_names.file_id%TYPE, my_data_id file_data.data_id%TYPE);
DROP FUNCTION IF EXISTS  create_file_link_data(bn bucket_names.bucket_name%TYPE, my_name file_names.name%TYPE, my_data_id file_data.data_id%TYPE);


COMMIT;
