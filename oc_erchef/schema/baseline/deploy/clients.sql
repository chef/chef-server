-- Deploy clients

BEGIN;

CREATE TABLE IF NOT EXISTS clients(
  id CHAR(32) PRIMARY KEY,
  org_id CHAR(32) NOT NULL,
  authz_id CHAR(32) NOT NULL UNIQUE,
  name TEXT NOT NULL,
  CONSTRAINT org_id_name_unique UNIQUE(org_id, name),
  public_key TEXT,
  validator BOOLEAN NOT NULL, -- DEFAULT FALSE
  last_updated_by CHAR(32) NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  pubkey_version SMALLINT NOT NULL, -- NEED A CONSTRAINT HERE, TOO
  admin BOOLEAN NOT NULL DEFAULT FALSE
);

COMMIT;
