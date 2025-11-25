-- Verify keys_by_name_add_disabled
-- Verify disabled column exists in keys_by_name view

BEGIN;

SELECT disabled FROM keys_by_name WHERE FALSE;

ROLLBACK;
