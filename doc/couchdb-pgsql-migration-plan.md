# Overview

This document is the evolving plan for migrating a single organization from `couchdb` to `postgresql`. Until further notice, this plan only covers migrating a single organization in preprod.

# 0. Preparation

## 0.1 Hosts and Access

Hosts that you'll need access to during the migration:
* couchdb-master
* opscode-lb (all external)
* opscode-lbint (internal master)

## 0.2 Staging

1. Run `chef-client` on `couchdb-master`

   We will need to run `chef-client` on the `couchdb-master` node to deploy the latest version of the `chef-mover` project, which will be handling the migration.

## 0.3 Pre-migration Loading

1. Restart Couchdb

   ```bash
   sudo /etc/init.d/couchdb restart
   ```

   Since we're going to be running a memory-heavy operation in loading the `opscode-account` database into a DETS table, we're going to restart `couchdb` in order to give us some head-room.

1. Start up chef-mover

   ```bash
   cd /srv/chef_mover/current
   bin/mover console
   ```

1. Load `opscode-account` Database into DETS Table

   From `mover` console:

   ```erlang
   moser_acct_processor:process_account_file().
   ```

   This should take roughly **10 minutes**. Alternatively, if you've previously loaded the account database into a DETS table, you can simply open the tables:

   ```erlang
   moser_acct_processor:open_account().
   ```

# 1. Migration

1. Run the Migration

   From `mover` console:

   ```erlang
   mover_org_migrator_sup:start_org_migrator("$ORG_NAME").
   ```
