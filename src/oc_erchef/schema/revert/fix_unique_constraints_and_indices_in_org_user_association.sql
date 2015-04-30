BEGIN;

ALTER TABLE org_user_associations ADD CONSTRAINT org_user_associations_org_id_key UNIQUE (org_id);
ALTER TABLE org_user_associations ADD CONSTRAINT org_user_associations_user_id_key UNIQUE (user_id);
DROP INDEX org_user_associations_index_user_id_key;

COMMIT;
