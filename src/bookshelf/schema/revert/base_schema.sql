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


COMMIT;
