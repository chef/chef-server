-- Verify cookbook_versions

BEGIN;

SELECT id, major, minor, patch, frozen,
       meta_attributes, meta_deps, meta_long_desc,
       metadata, serialized_object, updated_at,
       created_at, last_updated_by, cookbook_id
FROM cookbook_versions
WHERE FALSE;

ROLLBACK;
