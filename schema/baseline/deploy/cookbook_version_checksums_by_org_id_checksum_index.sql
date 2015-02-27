-- Deploy cookbook_version_checksums_by_org_id_checksum_index
-- requires: cookbook_version_checksums

BEGIN;

CREATE INDEX cookbook_version_checksums_by_org_id_checksum
  ON cookbook_version_checksums(org_id, checksum);

COMMIT;
