BEGIN;

ALTER TABLE org_user_invites ADD CONSTRAINT org_user_invites_org_id_key UNIQUE (org_id);
ALTER TABLE org_user_invites ADD CONSTRAINT org_user_invites_user_id_key UNIQUE (user_id);
DROP INDEX org_user_invites_composite_index_org_id_user_id_key;
DROP INDEX org_user_invites_index_user_id_key;

COMMIT;
