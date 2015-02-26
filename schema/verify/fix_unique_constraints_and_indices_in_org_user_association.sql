BEGIN;

SELECT 1/(1-count(*)) FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS where constraint_name = 'org_user_associations_org_id_key';
SELECT 1/(1-count(*)) FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS where constraint_name = 'org_user_associations_user_id_key';
SELECT 1/count(*) FROM pg_class WHERE relname='org_user_associations_index_user_id_key';

ROLLBACK;
