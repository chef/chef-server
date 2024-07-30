-- Deploy telemetry

BEGIN;

CREATE TABLE IF NOT EXISTS telemetry(
  property CHAR(32),
  value_string CHAR(32) NOT NULL,
  event_timestamp TIMESTAMP
);

COMMIT;