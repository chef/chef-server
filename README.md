# CHEF MOVER #

![Migration Visualization](http://images.memegenerator.net/instances/400x/10601974.jpg)

Orchestrate and execute the migration of nodes from couch to SQL.

## Installation ##

Add the `chef-mover` default recipe to run_list of the node where
you want to run the migration and run chef-client. You should now have
a working install with config based on the node's environment in
`/srv/mover`.

Review the config in `/srv/mover/etc/app.config`. While all of the
config settings matter, here's a run-down of those that are mover
specific.

* `dry_run`: When true, mover will not communicate with superconductor
  nor darklaunch. Data will be read from couchdb and written to MySQL,
  but org status and load balacing rules will not be changed.

* `preload_org_count`: used to set the number of orgs to load into
  read-through cache. For testing it can be useful to set this to a
  low number. For actual migration, set this to a number larger than
  total number of assigned orgs.

* `nginx_control_urls`: A list of URLs to all Erlang darklaunch serves
  Both standalone servers and those running embedded within erchef
  should be listed, for the latter the URL should end with
  '/darklaunch'

* `nginx_control_urls`: A list of URLs to superconductor apps for
  controlling nginx config. These URLs should end with '/configure'.

## Database Setup ##

Verify that the database you are configured to talk to has the correct
nodes table created. For testing, you might want to make use of the
following:

Schema setup:

    create database migration_test;
    CREATE USER 'dev'@'%' IDENTIFIED BY 'opensesame';
    GRANT ALL ON migration_test.* TO 'dev'@'%';

Table creation:

    CREATE TABLE `nodes` (
      `id` char(32) NOT NULL,
      `authz_id` char(32) NOT NULL,
      `org_id` char(32) NOT NULL,
      `name` varchar(255) NOT NULL,
      `environment` varchar(255) NOT NULL,
      `serialized_object` mediumblob,
      `last_updated_by` char(32) NOT NULL,
      `created_at` datetime NOT NULL,
      `updated_at` datetime NOT NULL,
      PRIMARY KEY (`id`),
      UNIQUE KEY `authz_id` (`authz_id`),
      UNIQUE KEY `org_id` (`org_id`,`name`),
      KEY `nodes_org_id_index` (`org_id`),
      KEY `nodes_org_id_environment_index` (`org_id`,`environment`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin

## Starting mover ##

Here's what you do to start mover. Starting mover will load all orgs
and start loading the read-through cache. Based on our test in prod,
this will negatively impact perf due to the additional load on couch
across all orgs.

    sudo su opscode
    cd /srv/mover
    bin/mover console

mover will tell you what it is doing. Leave it alone until preloading
orgs is complete. You can monitor progress by watching the log files
in `/srv/mover/log/mover_*.1`. You should see a message similar to the
following when preloading is complete:

    =INFO REPORT==== 28-Oct-2011::21:10:51 ===
    org summary after preloading: [{total,4432},
                                   {preloaded,4432},
                                   {read_only,0},
                                   {active,0},
                                   {migrated,0},
                                   {error,0}]

    =INFO REPORT==== 28-Oct-2011::21:10:51 ===
    preloading complete

mover will write the file `ORGS_ALREADY_LOADED` in the current working
directory once all unassigned orgs have been fetched from
CouchDB. When mover is restarted, the presence of this file prevents
the list of unassigned orgs from being re-fetched from CouchDB. This
is important because once a migration has started, new orgs are
assumed to be in the migrated state and we don't want to add them to
the unmigrated list.

## Starting the migration ##

In the mover console, you can start migration as follows:

    mover_manager:get_going(NumBatches, NumOrgs, NodeBatchSize).

We've tested using batches of 10 orgs and 100 node batch sizes. So you
could migrate the first 1000 orgs as:

    mover_manager:get_going(100, 10, 100).

Monitor the logs files to check progress. You can also get some status
info with the following commands:

    mover_manager:status().
    mover_status:summarize_orgs().

### checking on an org ###

Use the `mover_status` module to check on details for an org by name:

    mover_status:org_by_name(<<"userprimary">>).

    [{org,<<"60d3ed4da757402ea5dd6da9131baeef">>,
      <<"userprimary">>,true,false,false,false,undefined,[]}]

The return value contains an org record. Decode based on `mover.hrl`.

## mover achritecture and migration work flow ##

TODO
