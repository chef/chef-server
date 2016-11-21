-- Revert smart_delete

BEGIN;

DROP FUNCTION IF EXISTS mark_deleted(id file_data.data_id%TYPE);
DROP TRIGGER IF EXISTS delete_file_last_reference_on_delete ON files;
DROP TRIGGER IF EXISTS delete_file_last_reference_on_update ON files;
DROP FUNCTION IF EXISTS delete_file_last_reference();

DROP FUNCTION IF EXISTS purge_expired(delay integer);
DROP FUNCTION IF EXISTS cleanup_abandoned_uploads(delay integer);
DROP FUNCTION IF EXISTS mark_deleted(id file_data.data_id%TYPE);

COMMIT;
