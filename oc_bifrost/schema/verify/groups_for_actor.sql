-- Verify groups_for_actor

BEGIN;

SELECT pg_catalog.has_function_privilege(
  'groups_for_actor(bigint)',
  'execute');

ROLLBACK;
