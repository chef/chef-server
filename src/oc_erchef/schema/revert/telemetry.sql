-- Revert telemetry

BEGIN;

DROP TABLE IF EXISTS telemetry;

COMMIT;