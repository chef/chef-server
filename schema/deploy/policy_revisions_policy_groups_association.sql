-- Deploy policy_revisions_policy_groups_association

BEGIN;

CREATE TABLE IF NOT EXISTS policy_revisions_policy_groups_association(
  id VARCHAR(32) PRIMARY KEY,
  org_id CHAR(32) NOT NULL,

  policy_group_authz_id CHAR(32) NOT NULL,

  policy_revision_revision_id VARCHAR(255) NOT NULL,
  policy_revision_name VARCHAR(255) NOT NULL,

  policy_group_name VARCHAR(255) NOT NULL,

  last_updated_by CHAR(32) NOT NULL,

  -- policy_group_name, policy_authz_id references policy_groups (by org)
  -- abbreviate policy_revisions_policy_groups_association as prpga because of length limits
  CONSTRAINT prpga_association_policy_group_name_fkey
    FOREIGN KEY (org_id, policy_group_authz_id, policy_group_name)
    REFERENCES policy_groups(org_id, authz_id, name)
    ON UPDATE CASCADE ON DELETE CASCADE,

  -- policy_revision_name, policy_revision_id reference policy_revisions (by org)
  -- abbreviate policy_revisions_policy_groups_association as prpga because of length limits
  CONSTRAINT prpga_policy_name_revision_id_fkey
    FOREIGN KEY (org_id, policy_revision_name, policy_revision_revision_id)
    REFERENCES policy_revisions(org_id, name, revision_id)
    ON UPDATE CASCADE ON DELETE CASCADE,

  -- For a given policy_group, there can be at most one revision of a policy_name
  -- abbreviate policy_revisions_policy_groups_association as prpga because of length limits
  CONSTRAINT prpga_name_group_unique
    UNIQUE(org_id, policy_revision_name, policy_group_name),

  CONSTRAINT policy_revisions_org_id_fkey
    FOREIGN KEY (org_id)
    REFERENCES orgs(id)
    ON DELETE CASCADE
);

COMMIT;
