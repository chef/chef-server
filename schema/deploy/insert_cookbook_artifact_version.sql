-- Deploy insert_cookbook_artifact_version
-- requires: cookbook_artifact_version_checksums
-- requires: checksums

BEGIN;

CREATE OR REPLACE FUNCTION insert_cookbook_artifact_version(
  p_identifier cookbook_artifact_versions.identifier%TYPE,
  p_metadata cookbook_artifact_versions.metadata%TYPE,
  p_serialized_object cookbook_artifact_versions.serialized_object%TYPE,
  p_created_at cookbook_artifact_versions.created_at%TYPE,
  p_created_by cookbook_artifact_versions.created_by%TYPE,
  p_org_id cookbook_artifacts.org_id%TYPE,
  p_name cookbook_artifacts.name%TYPE,
  p_authz_id cookbook_artifacts.authz_id%TYPE,
  p_checksums char(32)[]
)
RETURNS SETOF cookbook_artifact_versions
LANGUAGE plpgsql
AS $$
DECLARE
  v_cookbook_artifact_id cookbook_artifacts.id%TYPE;
  v_cookbook_artifact_version cookbook_artifact_versions%ROWTYPE;
  v_checksum char(32);
BEGIN
  -- first let's create the cookbook_artifact record if needed
  SELECT id
  FROM cookbook_artifacts
  WHERE org_id = p_org_id
  AND name = p_name
  -- TODO: and authz_id = ?
  INTO v_cookbook_artifact_id;
  IF NOT FOUND THEN
    INSERT INTO cookbook_artifacts(org_id, name, authz_id)
    VALUES (p_org_id, p_name, p_authz_id)
    RETURNING id
    INTO v_cookbook_artifact_id;
  END IF;

  -- then create the cookbook_artifact_version record
  INSERT INTO cookbook_artifact_versions(identifier,
                                         metadata,
                                         serialized_object,
                                         created_at,
                                         created_by,
                                         cookbook_artifact_id)
  VALUES (p_identifier,
          p_metadata,
          p_serialized_object,
          p_created_at,
          p_created_by,
          v_cookbook_artifact_id)
  RETURNING cookbook_artifact_versions.*
  INTO v_cookbook_artifact_version;

  -- and finally we can proceed to creating the
  -- cookbook_artifact_version_checksum records,
  -- throwing a custom exception if a checksum is missing
  -- to allow for a graceful handling at the client's level
  BEGIN
    FOREACH v_checksum IN ARRAY p_checksums
    LOOP
      INSERT INTO cookbook_artifact_version_checksums(cookbook_artifact_version_id, org_id, checksum)
      VALUES (v_cookbook_artifact_version.id, p_org_id, v_checksum);
    END LOOP;
  EXCEPTION
    WHEN foreign_key_violation THEN
      RAISE EXCEPTION
      USING ERRCODE = 'CS001',
            MESSAGE = 'Missing checksum';
  END;

  RETURN NEXT v_cookbook_artifact_version;
END;
$$;

COMMIT;
