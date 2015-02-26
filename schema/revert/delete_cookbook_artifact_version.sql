-- Revert delete_cookbook_artifact_version

BEGIN;

DROP FUNCTION IF EXISTS delete_cookbook_artifact_version(
  p_cookbook_artifact_version_id cookbook_artifact_versions.id%TYPE
);

COMMIT;
