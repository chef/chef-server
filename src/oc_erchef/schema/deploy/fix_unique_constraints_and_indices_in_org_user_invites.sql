BEGIN;

ALTER TABLE org_user_invites DROP CONSTRAINT org_user_invites_org_id_key;
ALTER TABLE org_user_invites DROP CONSTRAINT org_user_invites_user_id_key;
CREATE UNIQUE INDEX org_user_invites_composite_index_org_id_user_id_key ON org_user_invites(org_id, user_id);
CREATE INDEX org_user_invites_index_user_id_key ON org_user_invites(user_id);

COMMIT;
