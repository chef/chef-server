+++
title = "Upgrade to Chef Infra Server 14"

date = 2022-02-04T15:29:44-08:00
draft = false

[menu]
  [menu.server]
    title = "Upgrade 14"
    identifier = "server/upgrade_14.md"
    parent = "server/upgrade"
    weight = 20
+++

Chef Infra Server 14.0 moved from Solr to Elasticsearch as its search index.
{{% server_upgrade_duration %}}

The Chef Infra Server 14 upgrade does not automatically reindex existing external Elasticsearch installations.

To upgrade the External PostgreSQL attached to Chef Infra Server, see the [Upgrade PostgreSQL]({{< relref "upgrade_postgresql">}})

## Standalone Server

{{% server_upgrade_duration %}}

1. Run `vacuumdb` before starting the upgrade:

   ```bash
   sudo su - opscode-pgsql
   /opt/opscode/embedded/bin/vacuumdb --all --full
   exit
   ```

   You should see output like:

   ```bash
   vacuumdb: vacuuming database "bifrost"
   vacuumdb: vacuuming database "oc_id"
   vacuumdb: vacuuming database "opscode-pgsql"
   vacuumdb: vacuuming database "opscode_chef"
   vacuumdb: vacuuming database "postgres"
   vacuumdb: vacuuming database "template1"
   ```

1. Back up your Chef Infra Server data before starting the upgrade process using [chef-server-ctl-backup]({{< relref "server_backup_restore#backup" >}}). **Make a note of where the backup is located** (The default is `/var/opt/chef-backup`). Please note that Chef Infra Server will go offline to perform the backup:

   ```bash
   sudo chef-server-ctl backup
   ```

1. Confirm that the Chef Infra Server services are operational:

   ```bash
   chef-server-ctl reconfigure
   ```

1. If you are already running 12.17.15 or greater, proceed to the next step. Otherwise consult the [upgrade matrix](#upgrade-matrix) and perform a stepped upgrade.

   - If you are running Chef Infra Server 12.3.0, upgrade to 12.17.15.
   - If you are running Chef Infra Server 11, first upgrade to 12.3.0, and then to 12.17.15.

   After performing the stepped upgrade to 12.17.15, continue with the next step.

1. Download the desired Chef Infra Server version from the [Chef Infra Server Downloads](https://www.chef.io/downloads/tools/infra-server).

1. Stop the Chef Infra Server:

   ```bash
   chef-server-ctl stop || chef-server-ctl kill rabbitmq
   ```

1. Install the Chef Infra Server package:

   To install with `dpkg`:

   ```bash
   dpkg -iEG /path/to/chef-server-core-VERSION.deb
   ```

   To install with the RPM Package Manager:

   ```bash
   rpm -Uvh --nopostun /path/to/chef-server-core-VERSION.rpm
   ```

1. Upgrade the server and accept the Chef Software license by entering `Yes` at the prompt:

   ```bash
   chef-server-ctl upgrade
   ```

   To accept the license and upgrade in one command:

   ```bash
   CHEF_LICENSE='accept' chef-server-ctl upgrade
   ```

1. If the upgrade failed, see the section below on how to handle an [upgrade failure](#upgrade-failure-troubleshooting).

   If the upgrade was successful, start Chef Infra Server:

   ```bash
   chef-server-ctl start
   ```

1. [Upgrade](#upgrading-manage-add-on) any Chef Infra Server add-ons.

1. After the upgrade process is complete, test and verify that the server works.

1. Clean up the Chef Infra Server by removing the old data:

   ```bash
   chef-server-ctl cleanup
   ```

1. Reindex the database:

   ```bash
   sudo su - opscode-pgsql
   /opt/opscode/embedded/bin/reindexdb --all
   ```

   You should see output like:

   ```bash
   reindexdb: reindexing database "bifrost"
   reindexdb: reindexing database "oc_i
   reindexdb: reindexing database "opscode-pgsql"
   reindexdb: reindexing database "opscode_chef"
   reindexdb: reindexing database "postgres"
   reindexdb: reindexing database "template1"
   ```

You are now finished with the upgrade.

