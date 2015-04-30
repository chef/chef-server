-- Deply delete_cookbook_artifact_version
-- requires: cookbook_artifact_version_checksums
-- requires: checksums

-- Deletes a cookbook artifact version, and returns
-- the checksums that should be deleted from storage

BEGIN;

CREATE OR REPLACE FUNCTION delete_cookbook_artifact_version(
  p_cookbook_artifact_version_id cookbook_artifact_versions.id%TYPE
)
RETURNS TABLE(checksum_to_delete char(32))
LANGUAGE plpgsql
AS $$
DECLARE
  v_cookbook_artifact_id cookbook_artifacts.id%TYPE;
  v_org_id cookbook_artifacts.org_id%TYPE;
  v_checksums_to_delete char(32)[];
BEGIN
  SELECT ca.id, ca.org_id
  FROM cookbook_artifacts AS ca
  JOIN cookbook_artifact_versions AS cav
  ON cav.cookbook_artifact_id = ca.id
  WHERE cav.id = p_cookbook_artifact_version_id
  INTO v_cookbook_artifact_id, v_org_id;

  -- first we need to delete the checksums, since we're going to delete
  -- the files from storage
  -- both from cookbook_artifact_version_checksums and checksums tables
  RETURN QUERY
  WITH artifact_version_checksums AS (
    DELETE FROM cookbook_artifact_version_checksums
    WHERE cookbook_artifact_version_id = p_cookbook_artifact_version_id
    RETURNING checksum
  ),
  -- then we delete the cookbook_artifact_version record
  delete_cookbook_artifact_version AS(
    DELETE FROM cookbook_artifact_versions
    WHERE id = p_cookbook_artifact_version_id
  ),
  -- and finally we delete the cookbook_artifact record if it has
  -- no more versions
  delete_cookbook_artifact AS(
    DELETE FROM cookbook_artifacts
    WHERE id = v_cookbook_artifact_id
    AND NOT EXISTS(
      SELECT *
      FROM cookbook_artifact_versions
      WHERE cookbook_artifact_id = v_cookbook_artifact_id
      AND id != p_cookbook_artifact_version_id
    )
  )
  DELETE FROM checksums AS c
  USING artifact_version_checksums
  WHERE c.checksum = artifact_version_checksums.checksum
  AND c.org_id = v_org_id
  -- we don't want to delete files if they're used by another
  -- cookbook artifact version
  AND NOT EXISTS(
    SELECT *
    FROM cookbook_artifact_version_checksums AS cavc
    WHERE cavc.checksum = c.checksum
    AND cavc.org_id = v_org_id
    AND cavc.cookbook_artifact_version_id != p_cookbook_artifact_version_id
  )
  -- nor if they're used by another cookbook version, for that matter
  AND NOT EXISTS(
    SELECT *
    FROM cookbook_version_checksums AS cvc
    WHERE cvc.checksum = c.checksum
    AND cvc.org_id = v_org_id
  )
  RETURNING c.checksum;
END;
$$;

COMMIT;
