BEGIN;

SELECT 1/(1-count(*)) FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS where constraint_name = 'org_user_invites_org_id_key';
SELECT 1/(1-count(*)) FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS where constraint_name = 'org_user_invites_user_id_key';
-- Tests that INDEX org_user_invites_composite_index_org_id_user_id_key exists and is unique
SELECT 1/count(*) FROM pg_index AS idx JOIN pg_class AS i ON i.oid = idx.indexrelid WHERE i.relname='org_user_invites_composite_index_org_id_user_id_key' and idx.indisunique;
SELECT 1/count(*) FROM pg_class WHERE relname='org_user_invites_index_user_id_key';

ROLLBACK;
