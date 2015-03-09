-- Deploy policy_groups
-- requires: policies

BEGIN;

ALTER TABLE policies DROP CONSTRAINT policies_name_group_org_id_key;
ALTER TABLE policies DROP COLUMN policy_group CASCADE;

ALTER TABLE policies ADD CONSTRAINT policies_name_org_id_unique UNIQUE(org_id, name);

CREATE TABLE IF NOT EXISTS policy_groups(
  id VARCHAR(32) PRIMARY KEY,
  org_id CHAR(32) NOT NULL,
  name VARCHAR(255) NOT NULL,
  authz_id CHAR(32) NOT NULL UNIQUE,
  last_updated_by CHAR(32) NOT NULL,
  serialized_object bytea,
  CONSTRAINT policy_groups_org_id_name_unique UNIQUE(org_id, name),
  CONSTRAINT policy_groups_org_id_authz_id_name_unique UNIQUE(org_id, authz_id, name),
  CONSTRAINT policies_org_id_fkey
    FOREIGN KEY (org_id)
    REFERENCES orgs(id)
    ON DELETE CASCADE
);


COMMIT;
