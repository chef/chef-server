-- Deploy node_external_storage
-- requires: nodes

BEGIN;

ALTER TABLE ONLY nodes ALTER COLUMN serialized_object SET STORAGE EXTERNAL;

COMMIT;
