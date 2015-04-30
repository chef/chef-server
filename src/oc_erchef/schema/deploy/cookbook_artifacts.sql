-- Deploy cookbook_artifacts

BEGIN;

CREATE TABLE IF NOT EXISTS cookbook_artifacts(
  id SERIAL PRIMARY KEY,
  org_id CHAR(32) NOT NULL,
  name TEXT NOT NULL,
  UNIQUE(org_id, name),
  authz_id CHAR(32) NOT NULL UNIQUE
);

CREATE INDEX cookbook_artifacts_org_id_index ON cookbook_artifacts(org_id);

COMMIT;
