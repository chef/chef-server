-- Revert cookbook_version_checksums_by_org_id_checksum_index

BEGIN;

DROP INDEX IF EXISTS cookbook_version_checksums_by_org_id_checksum;

COMMIT;
