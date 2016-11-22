-- Revert debug_object_acl_view

BEGIN;

DROP VIEW IF EXISTS debug.object_acl;

COMMIT;
