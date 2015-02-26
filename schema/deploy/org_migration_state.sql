-- Deploy org_migration_state

BEGIN;

CREATE TYPE org_state AS ENUM (
  'holding', -- this org will not be migrated
  'ready', -- this org is ready to migrate
  'started', -- migration for this org is in process
  'completed', -- migration compelted successfully
  'failed', -- migration failed. see fail_location for where.
  'purge_started',
  'purge_successful'
);

CREATE TABLE IF NOT EXISTS org_migration_state(
  id SERIAL PRIMARY KEY,
  org_name TEXT NOT NULL,
  org_id VARCHAR(36) NOT NULL UNIQUE,
  state org_state NOT NULL DEFAULT 'holding',
  fail_location VARCHAR(50),
  migration_start TIMESTAMP,
  migration_end TIMESTAMP
);

CREATE INDEX org_migration_state_name_idx ON org_migration_state(org_name);

COMMIT;
