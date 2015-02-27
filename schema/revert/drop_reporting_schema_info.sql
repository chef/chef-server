-- Revert drop_reporting_schema_info

BEGIN;

CREATE TABLE IF NOT EXISTS reporting_schema_info(
  version INTEGER NOT NULL DEFAULT 0
);

COMMIT;
