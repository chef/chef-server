-- Revert create_composite_key_org_migration_state

BEGIN;

ALTER TABLE org_migration_state DROP CONSTRAINT org_migration_state_pkey;
ALTER TABLE org_migration_state ADD CONSTRAINT org_migration_state_org_id_key UNIQUE (org_id);
ALTER TABLE org_migration_state ADD PRIMARY KEY (id);

COMMIT;
