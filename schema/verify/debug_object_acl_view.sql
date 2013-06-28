-- Verify debug_object_acl_view

BEGIN;

SELECT "object", "authorizee", "type", "permission", "directly_granted"
FROM debug.object_acl WHERE FALSE;

ROLLBACK;
