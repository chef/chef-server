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
   cd /srv/mover
   bin/mover/console
   ```

1. Load `opscode-account` Database into DETS Table

   From `mover` console:

   ```erlang
   moser_acct_processor:process_account_file().
   ```

   This should take roughly **INSERT_TIME_HERE** minutes.

## 0.4 Maintenance Preparation

1. Edit `xdarklaunch` DataBag

   ```bash
   export OPS_ENV=rs-preprod
   knife edit databags/xdarklaunch/$OPS_ENV.json
   ```

   In your editor, modify the following:

   ```javascript
   {
     "id": "rs-preprod",
     "xdarklaunch": {
       "dl_orgname": {
         // your orgname below
         "$ORG_NAME": {
           "503_mode": true
         }
       }
     }
   }
   ```

# 1. Migration

The clock for downtime starts as soon the following batch of `chef-client` runs finishes.

## 1.1 Initiate Maintenance Mode

1. Run `chef-client` on all `opscode-lb` Nodes

   _This applies to the internal and external nodes referenced above._

   ```bash
   sudo chef-client
   ```

## 1.2 Migration

1. Run the Migration

   From `mover` console:

   ```erlang
   %% TODO: this probably no longer works
   Db = moser_chef_processor:process_organization("$ORG_NAME") %% <- your orgname here
   moser_chef_converter:insert(Db).
   ```

## 1.3 Initiate Erchef Mode and Disable Maintenance Mode

1. Edit `xdarklaunch` DataBag

   ```bash
   export OPS_ENV=rs-preprod
   knife edit databags/xdarklaunch/$OPS_ENV.json
   ```

   In your editor, modify the following:

   ```javascript
   {
     "id": "rs-preprod",
     "xdarklaunch": {
       "dl_orgname": {
         // your orgname below
         "$ORG_NAME": {
           "couchdb_chef": false
           "503_mode": false // we could also remove the maintenance mode line
         }
       }
     }
   }
   ```

1. Run `chef-client` on all the `opscode-lb` Nodes

   ```bash
   sudo chef-client
   ```
