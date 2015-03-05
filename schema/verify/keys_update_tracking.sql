-- Verify update fields in keys table

BEGIN;
  SELECT last_updated_by, updated_at FROM keys WHERE FALSE;
ROLLBACK;
