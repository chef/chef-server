-- Revert telemetry

BEGIN;

DROP TABLE IF EXISTS telemetry;

DROP FUNCTION IF EXISTS telemetry_check_send;

COMMIT;