-- Deploy environments

BEGIN;

CREATE TABLE IF NOT EXISTS environments(
  id CHAR(32) PRIMARY KEY,
  authz_id CHAR(32) NOT NULL UNIQUE,
  org_id CHAR(32) NOT NULL,
  name TEXT NOT NULL,
  UNIQUE(org_id, name),
  last_updated_by CHAR(32) NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  serialized_object bytea
);

ALTER TABLE environments ALTER serialized_object SET STORAGE EXTERNAL;

COMMIT;
