-- Verify delete_cookbook_artifact_version

BEGIN;

SELECT has_function_privilege(
  'delete_cookbook_artifact_version(bigint)',
  'execute');

ROLLBACK;
