-- Deploy containers

BEGIN;

CREATE TABLE IF NOT EXISTS containers(
  id VARCHAR(36) PRIMARY KEY,
  org_id CHAR(32) NOT NULL,
  authz_id CHAR(32) NOT NULL UNIQUE,
  name TEXT NOT NULL,
  CONSTRAINT containers_org_id_name_key UNIQUE(org_id, name),
  last_updated_by CHAR(32) NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

COMMIT;
