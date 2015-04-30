-- Deploy widen_uuid_fields
-- requires: oss_chef:roles
-- requires: oss_chef:clients
-- requires: oss_chef:data_bags
-- requires: oss_chef:data_bag_items
-- requires: oss_chef:environments

BEGIN;

ALTER TABLE roles ALTER COLUMN id TYPE VARCHAR(36);
ALTER TABLE clients ALTER COLUMN id TYPE VARCHAR(36);
ALTER TABLE data_bags ALTER COLUMN id TYPE VARCHAR(36);
ALTER TABLE data_bag_items ALTER COLUMN id TYPE VARCHAR(36);
ALTER TABLE environments ALTER COLUMN id TYPE VARCHAR(36);

COMMIT;
