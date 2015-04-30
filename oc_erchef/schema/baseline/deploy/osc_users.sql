-- Deploy osc_users

BEGIN;

CREATE TABLE IF NOT EXISTS osc_users(
  id CHAR(32) PRIMARY KEY,
  authz_id CHAR(32) NOT NULL UNIQUE,
  username TEXT NOT NULL UNIQUE,
  email TEXT UNIQUE,
  public_key TEXT,

  hashed_password TEXT NOT NULL,
  salt TEXT NOT NULL,
  hash_type TEXT NOT NULL,

  last_updated_by CHAR(32) NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  external_authentication_uid TEXT,
  recovery_authentication_enabled BOOLEAN,
  admin BOOLEAN NOT NULL DEFAULT FALSE
);

COMMIT;
