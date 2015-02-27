-- Deploy nodes

BEGIN;

CREATE TABLE IF NOT EXISTS nodes(
  id CHAR(32) PRIMARY KEY,
  authz_id CHAR(32) NOT NULL UNIQUE,
  org_id CHAR(32) NOT NULL,
  name TEXT NOT NULL,
  UNIQUE(org_id, name),
  environment TEXT NOT NULL,
  last_updated_by CHAR(32) NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  serialized_object bytea
);

CREATE INDEX nodes_org_id_environment_index ON nodes(org_id, environment);

ALTER TABLE ONLY nodes ALTER COLUMN serialized_object SET STORAGE EXTERNAL;

COMMIT;
