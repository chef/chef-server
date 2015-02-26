-- Revert drop_osc_users

BEGIN;

-- We don't really need to put anything back, because we're not really
-- looking to provide migration back and forth between OSS and
-- Enterprise Chef.
--
-- Once it's gone, it's gone!

COMMIT;
