-- Deploy telemetry

BEGIN;

CREATE TABLE IF NOT EXISTS telemetry(
  property CHAR(512),
  value_string CHAR(512) NOT NULL,
  event_timestamp TIMESTAMP
);

CREATE OR REPLACE FUNCTION telemetry_check_send(
  hostname VARCHAR
)
RETURNS boolean
LANGUAGE plpgsql
AS $$
DECLARE
  host_timestamp telemetry.property%TYPE;
  last_send_timestamp telemetry.event_timestamp%TYPE;
BEGIN
  DELETE FROM telemetry
  WHERE property = hostname;

  INSERT INTO telemetry(property, value_string, event_timestamp)
  VALUES (hostname, '', current_timestamp);

  SELECT event_timestamp
  FROM telemetry
  WHERE property = 'last_send'
  INTO last_send_timestamp;
  IF NOT FOUND THEN
    INSERT INTO telemetry(property, value_string, event_timestamp)
    VALUES ('last_send', hostname, current_timestamp);
    RETURN true;
  END IF;
  IF last_send_timestamp < current_timestamp - interval '23 hours 54 minutes' THEN
    DELETE FROM telemetry
    WHERE property = 'last_send';
    INSERT INTO telemetry(property, value_string, event_timestamp)
    VALUES ('last_send', hostname, current_timestamp);
    RETURN true;
  END IF;
  RETURN false;
END;
$$;

COMMIT;