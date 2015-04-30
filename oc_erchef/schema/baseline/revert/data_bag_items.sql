-- Revert data_bag_items

BEGIN;

DROP TABLE IF EXISTS data_bag_items;

COMMIT;
