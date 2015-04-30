-- Verify migrate_org_migration_status_to_migratation_type

BEGIN;

SELECT 1/case_result.CASE FROM (SELECT ct, CASE  WHEN ct>0 THEN 0 ELSE 1 END from (SELECT COUNT(*) as ct FROM org_migration_state WHERE state = 'purge_successful') as ct) as case_result;

Rollback;
