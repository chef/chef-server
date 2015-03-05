-- Revert node_external_storage

BEGIN;

ALTER TABLE ONLY nodes ALTER COLUMN serialized_object SET STORAGE EXTENDED;

COMMIT;
