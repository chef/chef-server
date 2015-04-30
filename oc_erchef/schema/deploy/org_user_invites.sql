-- Deploy org_user_invites table
-- This was formerly known as association requests

BEGIN;

CREATE TABLE IF NOT EXISTS org_user_invites(
  id CHAR(32) PRIMARY KEY, -- ID of request; handed out for later use when accepting

  -- Historical note:
  -- The AssociationRequest document in couchdb used the couchdb document id of the org, not the GUID.
  -- The GUID is what's used everywhere else to identify the org; and for consistiency it's used here.
  org_id CHAR(32) NOT NULL UNIQUE, -- GUID of org.
  user_id VARCHAR(36) NOT NULL UNIQUE, -- ID of USER

  -- An association invite becomes invalid if the org or user is deleted.
  -- It is likely that we will need to cleanup some stale documents when migrating from couch
  FOREIGN KEY (org_id)
    REFERENCES orgs(id)
    ON DELETE CASCADE,

  FOREIGN KEY (user_id)
    REFERENCES users(id)
    ON DELETE CASCADE,

  -- The authz_id of the inviter. Formerly known as organization_admin_actor_id,
  -- this is the actor id used to perform the association when it is accepted.
  last_updated_by CHAR(32) NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  -- this isn't terribly useful, but kept for consistency with other tables.
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

COMMIT;
