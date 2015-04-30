-- Revert cookbook_versions_by_rank

BEGIN;

DROP VIEW IF EXISTS cookbook_versions_by_rank;

COMMIT;
