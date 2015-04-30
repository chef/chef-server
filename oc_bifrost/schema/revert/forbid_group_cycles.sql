-- Revert forbid_group_cycles

BEGIN;

DROP TRIGGER IF EXISTS no_long_range_cycles ON group_group_relations;

DROP FUNCTION IF EXISTS forbid_group_cycles();

COMMIT;
