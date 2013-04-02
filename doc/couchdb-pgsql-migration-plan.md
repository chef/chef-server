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
   # todo: update with correct command
   /srv/chef-mover/current/insert-command-here
   ```

1. Load `opscode-account` Database into DETS Table

   ```erlang
   moser> moser_acct_processor:process_account_file().
   ```

   This should take roughly **INSERT_TIME_HERE** minutes.

## 0.4 Maintenance Preparation

1. Edit `darklaunch` DataBag

   ```bash
   > export OPS_ENV=rs-preprod
   > knife edit databags/darklaunch/$OPS_ENV.json
   ```

   In your editor, modify the following:

   ```javascript
   {
     "id": "rs-preprod",
     "xdarklaunch": {
       "dl_orgname": {
         // your orgname below
         "$ORG_NAME": {
           "maintenance_mode": true
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
   > sudo chef-client
   ```

## 1.2 Migration

## 1.3 Initiate Erchef Mode

Erchef mode (`couchdb_chef = false` in `xdarklaunch`) is fully initiated before downtime is reversed to prevent the case where a single load balancer finishes its configuration before the rest and starts serving successful requests for an organization before the others are able to.

1. Edit `darklaunch` DataBag

   ```bash
   > export OPS_ENV=rs-preprod
   > knife edit databags/darklaunch/$OPS_ENV.json
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
         }
       }
     }
   }
   ```

1. Run `chef-client` on all the `opscode-lb` Nodes

   ```bash
   > sudo chef-client
   ```

## 1.4 Remove Maintenance Mode

1. Edit `darklaunch` DataBag

   ```bash
   > export OPS_ENV=rs-preprod
   > knife edit databags/darklaunch/$OPS_ENV.json
   ```

   In your editor, modify the following:

   ```javascript
   {
     "id": "rs-preprod",
     "xdarklaunch": {
       "dl_orgname": {
         // your orgname below
         "$ORG_NAME": {
           "maintenance_mode": false // we could also remove the maintenance mode line
         }
       }
     }
   }
   ```

1. Run `chef-client` on all the `opscode-lb` Nodes

   ```bash
   > sudo chef-client
   ```