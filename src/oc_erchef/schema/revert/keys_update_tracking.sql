-- Revert keys_update_tracking

BEGIN;

ALTER TABLE keys
  DROP COLUMN last_updated_by,
  DROP COLUMN updated_at;

COMMIT;
