-- Deploy drop_osc_users
-- requires: oss_chef:osc_users

BEGIN;

-- This is where users live in Open Source Chef; we have a different
-- schema for Enterprise Chef, though, so we can just nuke this table;
DROP TABLE IF EXISTS osc_users;

COMMIT;
