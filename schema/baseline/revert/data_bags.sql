-- Revert data_bags

BEGIN;

DROP TABLE IF EXISTS data_bags;

COMMIT;
