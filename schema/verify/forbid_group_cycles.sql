-- Verify forbid_group_cycles

BEGIN;

SELECT pg_catalog.has_function_privilege(
  'forbid_group_cycles()',
  'execute');

SELECT 1/COUNT(*) FROM information_schema.triggers
WHERE trigger_name = 'no_long_range_cycles'
  AND event_object_table = 'group_group_relations'
  AND event_manipulation = 'UPDATE';

SELECT 1/COUNT(*) FROM information_schema.triggers
WHERE trigger_name = 'no_long_range_cycles'
  AND event_object_table = 'group_group_relations'
  AND event_manipulation = 'INSERT';

ROLLBACK;
