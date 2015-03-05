BEGIN;

ALTER TABLE org_user_associations DROP CONSTRAINT org_user_associations_org_id_key;
ALTER TABLE org_user_associations DROP CONSTRAINT org_user_associations_user_id_key;
CREATE INDEX org_user_associations_index_user_id_key ON org_user_associations(user_id);

COMMIT;
