BEGIN;

CREATE TABLE IF NOT EXISTS policies(
  id VARCHAR(32) PRIMARY KEY,
  name VARCHAR(256) NOT NULL,
  policy_group VARCHAR(256),
  authz_id CHAR(32) NOT NULL UNIQUE,
  org_id CHAR(32) NOT NULL,
  last_updated_by CHAR(32) NOT NULL,
  serialized_object bytea,
  CONSTRAINT policies_name_group_org_id_key UNIQUE(name, policy_group, org_id),
  CONSTRAINT policies_org_id_fkey
    FOREIGN KEY (org_id)
    REFERENCES orgs(id)
    ON DELETE CASCADE
);

COMMIT
