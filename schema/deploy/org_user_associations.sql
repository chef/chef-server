-- Deploy org_user_associations table

BEGIN;

CREATE TABLE IF NOT EXISTS org_user_associations(
  -- Historical note:
  -- The AssociationRequest document in couchdb used the couchdb document id of the org, not the GUID.
  -- The GUID is what's used everywhere else to identify the org; and for consistiency it's used here.
  org_id CHAR(32) NOT NULL UNIQUE, -- GUID of org.
  user_id VARCHAR(36) NOT NULL UNIQUE, -- ID of USER

  PRIMARY KEY (org_id, user_id),

  -- An association becomes invalid if the org or user is deleted.
  -- It is likely that we will need to cleanup some stale documents when migrating from couch
  FOREIGN KEY (org_id)
    REFERENCES orgs(id)
    ON DELETE CASCADE,

  FOREIGN KEY (user_id)
    REFERENCES users(id)
    ON DELETE CASCADE,

  last_updated_by CHAR(32) NOT NULL, -- this ends up being the authz id of the inviter
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  -- this isn't terribly useful, but kept for consistency with other tables. (if we extend this table to have more useful info, that will change)
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

COMMIT;
