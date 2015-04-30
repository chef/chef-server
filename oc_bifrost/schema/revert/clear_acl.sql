-- Revert clear_acl

BEGIN;

DROP FUNCTION IF EXISTS clear_acl(auth_type, character, auth_permission);

COMMIT;
