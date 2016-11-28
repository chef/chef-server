-- Revert groups_for_actor

BEGIN;

DROP FUNCTION IF EXISTS groups_for_actor(BIGINT);

COMMIT;
