-- Revert update_acl

BEGIN;

DROP FUNCTION IF EXISTS update_acl(auth_type, character, auth_permission, character[], character[]);

COMMIT;
