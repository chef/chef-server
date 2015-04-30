-- Deploy policy_revisions
-- requires: policies

BEGIN;

-- Destructive, but this is okay because the feature is only available via
-- darklaunch with numerous feature flags and warnings (including warnings
-- about possible future data deletion).
DELETE FROM policies;
ALTER TABLE policies DROP COLUMN serialized_object CASCADE;
ALTER TABLE policies ADD CONSTRAINT policies_org_id_authz_id_name_unique UNIQUE(org_id, authz_id, name);

CREATE TABLE IF NOT EXISTS policy_revisions(
  id VARCHAR(32) PRIMARY KEY,
  org_id CHAR(32) NOT NULL,
  policy_authz_id CHAR(32) NOT NULL,
  revision_id VARCHAR(255) NOT NULL,
  name VARCHAR(255) NOT NULL,


  last_updated_by CHAR(32) NOT NULL,
  serialized_object bytea,

  CONSTRAINT policy_revisions_org_id_authz_id_policy_name_fkey
    FOREIGN KEY (org_id, policy_authz_id, name)
    REFERENCES policies(org_id, authz_id, name)
    ON UPDATE CASCADE ON DELETE CASCADE,

  CONSTRAINT policy_revisions_org_id_name_revision_id_unique UNIQUE(org_id, name, revision_id),

  CONSTRAINT policy_revisions_org_id_fkey
    FOREIGN KEY (org_id)
    REFERENCES orgs(id)
    ON DELETE CASCADE
);

COMMIT;
