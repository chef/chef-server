-- Verify data_bag_items

BEGIN;

SELECT id, org_id, data_bag_name,
       item_name, last_updated_by, created_at,
       updated_at, serialized_object
FROM data_bag_items
WHERE FALSE;

ROLLBACK;
