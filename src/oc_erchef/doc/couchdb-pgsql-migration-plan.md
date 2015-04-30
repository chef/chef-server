# Overview

This document is the evolving plan for migrating a single organization from `couchdb` to `postgresql`. Until further notice, this plan only covers migrating a single organization in preprod.

# 0. Preparation

## 0.1 Staging

1. Verify Code Deploys

   * opscode-chef - rel-2.9.0
   * opscode-erchef - 0.20.3
   * opscode-account - rel-1.32.1
   * opscode-lb - Cookbook Version 1.3.5
   * opscode-lb-int - Cookbook Version 1.3.5
   * chef-mover - 1.0.17

1. Verify Mover Dry-Run is Off

   Neither of the couchdb nodes (which run mover) should have the `mover-dry-run` tag.

   ```bash
   knife search node role:couchdb -a tags
   ```

   To double-check, ssh to the active couchdb node and verify that the dry_ryn config is false:

   ```bash
   grep dry_run /srv/chef_mover/current/etc/sys.config
   ```

   If not, run `chef-client`.

## 0.2 Pre-migration Loading

1. Restart Couchdb

   ```bash
   sudo /etc/init.d/couchdb restart
   ```

   Since we're going to be running a memory-heavy operation in loading the `opscode-account` database into a DETS table, we're going to restart `couchdb` in order to give us some head-room.

1. Start Up chef-mover

   ```bash
   cd /srv/chef_mover/current
   bin/mover console
   ```

1. Load `opscode-account` Database into DETS Table

   From `mover` console:

   ```erlang
   moser_acct_processor:process_account_file().
   ```

1. Reset Customer Organizations

   We've been test-migrating customer orgs with dry-run in production for the past week. It's time to clean up the data in SQL. From the `mover` console:

   ```erlang
   mover_util:reset_org("__INSERT_ORGNAME_HERE__"). %% I'm not putting the orgname here because you better not copy-pasta this
   ```

   To double-check the state of the recently reset organization, head over to the active chef-pgsql box. From `psql`:

   ```sql
   select * from org_migration_state where org_name = '__INSERT_ORGNAME_HERE__';
   ```

   The state should be set to `'holding'`.

# 1. Migration

1. Run the Migration

   From `mover` console:

   ```erlang
   mover_org_migrator_sup:start_org_migrator("__INSERT_ORGNAME_HERE__").
   ```

# 2. Migration Rollback - Disaster Recovery

In the event of a SEV1 issue with one of the initial migrated customers and they cannot make forward progress with their infrastructure, we reserve the option to unmigrate an organization. This is a last-resort option and should be considered carefully before proceeding. Any data that has been successfully saved after the migration will be lost.

To make this operation as quick as possible, open connections to the following nodes:

* active mysql
* active couchdb

1. Initialize 503 Mode

   From the active mysql node, run `/opt/redis/bin/redis-cli`:

   ```
   HSET dl_org_$ORGNAME 503_mode true
   ```

1. Reset and Re-Index

   From the active couchdb node in `mover console`:

   ```erlang
   mover_util:reset_org("__INSERT_ORGNAME_HERE__").
   moser_chef_reindex:reindex_org("__INSERT_ORGNAME_HERE__").
   ```

1. End 503 Mode and Re-Route to CouchDB / opscode-chef

   From `redis-cli` on the active mysql node:

   ```
   DEL dl_org_$ORGNAME
   ```
