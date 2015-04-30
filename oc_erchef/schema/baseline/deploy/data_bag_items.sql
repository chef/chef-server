-- Deploy data_bag_items
-- requires: data_bags

BEGIN;

CREATE TABLE IF NOT EXISTS data_bag_items(
  id CHAR(32) PRIMARY KEY,
  org_id CHAR(32) NOT NULL,
  data_bag_name TEXT NOT NULL,

  CONSTRAINT data_bag_items_org_id_fkey
    FOREIGN KEY (org_id, data_bag_name)
    REFERENCES data_bags(org_id, name)
    ON UPDATE CASCADE ON DELETE CASCADE,

  item_name TEXT NOT NULL,
  UNIQUE(org_id, data_bag_name, item_name),
  last_updated_by CHAR(32) NOT NULL,
  created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
  serialized_object bytea
);

ALTER TABLE data_bag_items ALTER serialized_object SET STORAGE EXTERNAL;

COMMIT;
