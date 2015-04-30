-- Deploy create_composite_key_org_migration_state

BEGIN;

ALTER TABLE org_migration_state DROP CONSTRAINT org_migration_state_pkey;
ALTER TABLE org_migration_state DROP CONSTRAINT org_migration_state_org_id_key;
ALTER TABLE org_migration_state ADD PRIMARY KEY (org_id, migration_type);

COMMIT;
