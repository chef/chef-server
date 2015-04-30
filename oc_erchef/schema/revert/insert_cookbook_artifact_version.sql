-- Revert insert_cookbook_artifact_version

BEGIN;

DROP FUNCTION IF EXISTS insert_cookbook_artifact_version(
  p_identifier cookbook_artifact_versions.identifier%TYPE,
  p_metadata cookbook_artifact_versions.metadata%TYPE,
  p_serialized_object cookbook_artifact_versions.serialized_object%TYPE,
  p_created_at cookbook_artifact_versions.created_at%TYPE,
  p_created_by cookbook_artifact_versions.last_updated_by%TYPE,
  p_org_id cookbook_artifacts.org_id%TYPE,
  p_name cookbook_artifacts.name%TYPE,
  p_authz_id cookbook_artifacts.authz_id%TYPE,
  p_checksums char(32)[]
);

COMMIT;
