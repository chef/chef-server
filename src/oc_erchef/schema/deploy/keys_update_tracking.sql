-- add new columns for tracking update information about keys
-- requires: multiple_keys

BEGIN;

ALTER TABLE keys
  ADD COLUMN last_updated_by CHARACTER(32) NOT NULL DEFAULT '',
  ADD COLUMN updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP;

COMMIT;

