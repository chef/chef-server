-- Deploy policy_revisions
-- requires: policies

BEGIN;

-- Destructive, but this is okay because the feature is only available via
-- darklaunch with numerous feature flags and warnings (including warnings
-- about possible future data deletion).
ALTER TABLE policies DROP COLUMN serialized_object CASCADE;

CREATE TABLE IF NOT EXISTS policy_revisions(
  id VARCHAR(32) PRIMARY KEY,
  org_id CHAR(32) NOT NULL,
  revision_id VARCHAR(255) NOT NULL,
  name VARCHAR(255) NOT NULL,


  last_updated_by CHAR(32) NOT NULL,
  serialized_object bytea,

  CONSTRAINT policy_revisions_org_id_policy_name_fkey
    FOREIGN KEY (org_id, name)
    REFERENCES policies(org_id, name)
    ON UPDATE CASCADE ON DELETE CASCADE,

  CONSTRAINT policy_revisions_org_id_name_revision_id_unique UNIQUE(org_id, name, revision_id),

  CONSTRAINT policy_revisions_org_id_fkey
    FOREIGN KEY (org_id)
    REFERENCES orgs(id)
    ON DELETE CASCADE
);

COMMIT;
