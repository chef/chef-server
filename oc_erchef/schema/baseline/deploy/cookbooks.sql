-- Deploy cookbooks

BEGIN;

CREATE TABLE IF NOT EXISTS cookbooks(
  id SERIAL PRIMARY KEY,
  org_id CHAR(32) NOT NULL,
  name TEXT NOT NULL,
  UNIQUE(org_id, name),
  authz_id CHAR(32) NOT NULL UNIQUE
);

CREATE INDEX cookbooks_org_id_index ON cookbooks(org_id);

COMMIT;
