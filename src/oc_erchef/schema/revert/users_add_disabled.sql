-- Revert users::users_add_disabled from oc_erchef

BEGIN;

ALTER TABLE users DROP COLUMN disabled;

COMMIT;
