-- Deploy orgs table

BEGIN;

CREATE TABLE IF NOT EXISTS orgs(
  id CHAR(32) PRIMARY KEY, -- GUID of org.
  authz_id CHAR(32) NOT NULL UNIQUE,
  name TEXT NOT NULL UNIQUE,
  -- This hasn't previously been required to be unique; Pre-created orgs have 'Pre-created' as their full_name
  full_name TEXT NOT NULL,

  -- The couchdb version of this had some fields for billing:
  -- billing_plan, chargify_customer_id and chargify_subscription_id, org_type
  -- Removed because they are no longer useful

  -- If we are creating organizations on demand, we should get rid of this field
  assigned_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  last_updated_by CHAR(32) NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

COMMIT;
