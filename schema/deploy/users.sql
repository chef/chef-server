-- Deploy users
-- requires: drop_osc_users

BEGIN;

CREATE TABLE IF NOT EXISTS users(
  id CHAR(32) PRIMARY KEY,
  authz_id CHAR(32) NOT NULL UNIQUE,
  username TEXT NOT NULL UNIQUE,
  email TEXT UNIQUE,
  pubkey_version INTEGER NOT NULL,
  -- Constraint on above?
  public_key TEXT, -- can't be null, right?
  serialized_object TEXT, -- not null, right
  last_updated_by CHAR(32) NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  external_authentication_uid TEXT,
  recovery_authentication_enabled BOOLEAN,
  admin BOOLEAN NOT NULL DEFAULT FALSE
);

COMMIT;
