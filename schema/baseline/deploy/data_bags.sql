-- Deploy data_bags

BEGIN;

CREATE TABLE IF NOT EXISTS data_bags(
  id CHAR(32) PRIMARY KEY,
  authz_id CHAR(32) NOT NULL UNIQUE,
  org_id CHAR(32) NOT NULL,
  name TEXT NOT NULL,
  UNIQUE(org_id, name),
  last_updated_by CHAR(32) NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

COMMIT;
