+++
title = "Upgrade Chef Infra Server"
draft = false

gh_repo = "chef-server"

aliases = ["/upgrade_server.html", "/upgrade_server/", "/upgrades/"]

[menu]
  [menu.server]
    title = "Upgrades"
    identifier = "server/installation/Upgrades"
    parent = "server/installation"
    weight = 60
+++

Each new release of Chef Infra Server improves reliability and updates 3rd party components to ensure the security of the server. It is important to keep Chef Infra Server up to date in order to ensure the secure and reliable operation of Chef Infra in your organization.

{{< warning >}}
Before upgrading a production server make sure to upgrade a test server to confirm the process.
{{< /warning >}}

## Upgrade Matrix

If running a Chef Infra Server 12.17.15 or later you can upgrade directly to the latest releases of Chef Infra Server 14. If you are running a release before 12.17.15 you must perform a stepped upgrade as outlined below.

| Running Version | Upgrade To Version | Requires License | Supported Version |
|---------|---------|------|-----------|
| 13 | 14 | Yes | Yes |
| 12.17.15 | 14 | Yes | No |
| 12.3.0 | 12.17.15 | No | No |
| 11 | 12.3.0 | No | No |

Requires License
: Chef Infra Server 13 and later are governed by the [Chef EULA]({{< relref "chef_license" >}}). You will be required to accept these terms when using Chef Infra Server for the first time by entering `Yes` when prompted.

Supported Release
: Chef Infra Server 13 and later are currently supported Chef Software releases. Earlier releases are no longer supported as of 12/31/2020. For more information about supported Chef Software see the [Supported Versions]({{< relref "/versions#supported-commercial-distributions" >}}) documentation.

## Release-Specific Steps

### Upgrading to 14.x

Chef Infra Server 14.0 moved from Solr to Elasticsearch as its search index.
{{% server_upgrade_duration %}}

The Chef Infra Server 14 upgrade does not automatically reindex existing external Elasticsearch installations.

#### Upgrading to 14.8

Chef Infra Server 14.8 upgrades PostgreSQL from 9.6 to 13.3. The 14.8 upgrade process requires a one-time downtime to vacuum, upgrade, and re-index the database. The entire upgrade operation takes approximately one minute per 1000 nodes. This process may take longer depending on your server hardware and the size of the node objects on your Chef Infra Server.

##### Database Preparation

1. Run `VACUUM FULL` on the PostgreSQL database if you don't have automatic vacuuming set up. This process will reduce the size of the database by deleting unnecessary data and speeds up the migration. The `VACUUM FULL` operation takes around 1 to 2 minutes per gigabyte of data depending on the complexity of the data, and requires free disk space at least as large as the size of your database.

    ```bash
    /opt/opscode/embedded/bin/vacuumdb --all --full
    ```

1. Back up the PostgreSQL database before upgrading so you can restore the full database to a previous release in the event of a failure. See [Backup and Restore]({{< relref "server_backup_restore" >}}) for additional information.

##### Upgrade Steps

Follow the Chef Infra Server upgrade instructions below.

##### Post-Upgrade Steps

Reindex the Chef Infra Server:

```bash
/opt/opscode/embedded/bin/reindexdb --all
```

{{< note >}}

Estimates are based on an 8-core 32 GB memory (t3.2xlarge) AWS EC2 instance with 2 organizations and 3 users having 565,000+ nodes, 13,000+ cookbooks, 29,000+ databags, 274,000+ environments, and 281,000+ roles.

{{</note >}}

### Upgrading to 12.17.15

{{< warning >}}
Upgrade Chef Infra Server and any add-ons to compatible versions before setting `insecure_addon_compat` to `false`.
{{< /warning >}}

As of version 12.14, Chef Infra Server renders passwords inside of the `/etc/opscode` directory by default.
If you are using Chef Infra Server without add-ons, or if you are using the latest add-ons versions, you can set `insecure_addon_compat` to `false` in `/etc/opscode/chef-server.rb.
and Chef Infra Server will write all credentials to a single location.

For more information on password generation, including a list of supported add-on versions, see [Chef Infra Server Credentials Management]({{< relref "server_security/#chef-infra-server-credentials-management" >}}).

### Upgrading to 12.3.0

If you are running a Chef Infra Server release before 12.3.0, please contact Chef Support for guidance on upgrading your Chef Infra Server installation.

## Chef Infra Server 14 Upgrade Process

### Standalone Server

{{% server_upgrade_duration %}}

#### Standalone Upgrade Steps

1. Run `vacuumdb` before starting the upgrade:

   ```bash
   sudo su postgres
   /usr/bin/vacuumdb --all --full
   exit
   ```

1. Back up your Chef Infra Server data before starting the upgrade process using [chef-server-ctl-backup]({{< relref "server_backup_restore#backup" >}}). **Make a note of where the backup was placed** (normally under `/var/opt/chef-backup`). Please note that Chef Infra Server will go offline to perform the backup:

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

1. Download the desired Chef Infra Server version from the [Chef Infra Server Downloads](https://downloads.chef.io/products/chef-server).

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
   /usr/bin/reindexdb --all
   ```

   You are now finished with the upgrade.

{{< note >}}

Check the [post upgrade steps](#post-upgrade-steps) if you are upgrading from a version before Chef Infra Server 14.8 to a version greater than or equal to 14.8.

{{</note >}}

#### Upgrade Failure Troubleshooting

1. If the upgrade failed and you have a corrupted Chef Infra Server and/or a corrupted database, **DO NOT RISK YOUR BACKUP OF THE DATABASE.** Take all steps necessary to preserve the backup, including copying it to another disk. Consult with a professional sysadmin for instructions and best practices.

1. Contact customer support.

1. Reinstall the original version of Chef Infra Server you were using before attempting the upgrade process (if you had to perform a stepped upgrade, [install your original version of Chef Infra Server]({{< relref "install_server" >}}) before the stepped upgrade, not any versions you upgraded to in the stepped upgrade process). Again, **DO NOT RISK YOUR BACKUP OF THE DATABASE.** For example, consider using a separate disk from your backup for the new installation.

1. Consult the [restore documentation]({{< relref "server_backup_restore" >}}) and restore the database from the path where it was saved:

   ```bash
   chef-server-ctl restore /path/to/tar/archive.tar.gz
   ```

### External PostgreSQL

1. Log into the external PostgreSQL machine.

1. Run `vacuumdb` before starting the upgrade:

   ```bash
   sudo su postgres
   /usr/bin/vacuumdb --all --full
   exit
   ```

1. Log into the Chef Infra Server machine.

1. Consult the documentation on [`knife-ec-backup`](https://blog.chef.io/migrating-chef-server-knife-ec-backup-knife-tidy).

   If it is not already installed, install `knife-ec-backup`. A sample session follows
   (note that your steps could differ, depending on the versions of your software,
   the topology of your setup, your OS and distribution, and a range of other factors).

   For example:

   ```bash
   apt-get update
   apt install ruby
   apt install make
   curl -L https://chef.io/chef/install.sh | sudo bash -s -- -P chefdk
   export PATH=$PATH:/root/.chefdk/gem/ruby/2.6.0/bin
   apt-get -y install gcc postgresql libpq-dev
   /opt/chefdk/embedded/bin/gem install knife-ec-backup -- --with-pg-config=/opt/opscode/embedded/postgresql/9.6/bin/pg_config
   ```

1. Configure `knife` if it is not already configured. A sample session follows (again, note that your steps could differ, depending on a range of factors).

   For Example:

   ```bash
   $ chef-server-ctl org-create 4thcafe 'Fourth Cafe, Inc.' --association_user janedoe --filename /tmp/4thcafe-validator.pem
   $ chef generate repo chef-repo
   $ cd chef-repo/
   $ mkdir -p .chef
   $ echo '.chef' >> .gitignore
   $ cp /tmp/4thcafe-validator.pem .chef
   $ cp /home/ubuntu/janedoe.pem .chef
   $ cd .chef
   $ knife configure
   WARNING: No knife configuration file found. See https://docs.chef.io/config_rb/ for details.
   Please enter the chef server URL: [https://1.2.3.4/organizations/myorg] https://1.2.3.4/organizations/4thcafe
   Please enter an existing username or clientname for the API: [ubuntu] janedoe
   $ knife ssl fetch
   ```

1. Backup the database. For Example:

   ```bash
   mkdir /backup
   /opt/chefdk/embedded/bin/knife ec backup /backup
   ```

1. If you are running Chef Infra Server version 12.17.15 or greater, proceed to the next step below. Otherwise consult the [upgrade matrix](#upgrade-matrix) and perform a stepped upgrade.

   If you are running a Chef Infra Server release before 12.17.15, you cannot upgrade directly to 14.8.X. You must perform a stepped upgrade first.

   - If you are running Chef Infra Server 12.3.0, upgrade to 12.17.15.
   - If you are running Chef Infra Server 11, you must first upgrade to 12.3.0, and then to 12.17.15.

   After performing the stepped upgrade, return here and continue with the next step below.

1. [Download](https://downloads.chef.io/tools/infra-server) the Chef Infra Server version 14.8.X upgrade package.

1. Stop services.

   ```bash
   sudo chef-server-ctl stop || sudo chef-server-ctl kill rabbitmq
   ```

1. Install the Chef Infra Server 14.8.X package.

   dpkg:

   ```bash
   sudo dpkg -iEG /path/to/chef-server-core-VERSION.deb
   ```

   RPM:

   ```bash
   sudo rpm -U /path/to/chef-server-core-VERSION.rpm
   ```

1. Upgrade Chef Infra Server.

   ```bash
   sudo CHEF_LICENSE='accept' chef-server-ctl upgrade
   ```

1. If the upgrade failed, see the section on [upgrade failure troubleshooting](#upgrade-failure-troubleshooting-1).

   If the upgrade was successful, start the Chef Infra Server services and cleanup.

   ```
   sudo chef-server-ctl start
   sudo chef-server-ctl cleanup
   ```

   You are now finished with the Chef Infra Server upgrade. Proceed directly to the **Upgrade PostgreSQL** section.

**Upgrade PostgreSQL**

1. Log into the external PostgreSQL machine.

1. Update packages and install PostgreSQL 13.3.
   Example (Ubuntu):
   ```
   sudo apt-get update
   sudo apt-get install postgresql-13
   ```

1. Check if there are any differences in the config files. Make sure to update the new config files if required.

   ```
   diff /etc/postgresql/OLD_POSTGRESQL_VERSION/main/postgresql.conf /etc/postgresql/13/main/postgresql.conf
   diff /etc/postgresql/OLD_POSTGRESQL_VERSION/main/pg_hba.conf     /etc/postgresql/13/main/pg_hba.conf
   ```

1. Stop the PostgreSQL service.

   ```bash
   sudo systemctl stop postgresql.service
   ```

1. Log in as the PostgreSQL user.

   ```bash
   su postgres
   ```

1. Check clusters (notice the `--check` argument, this will not change any data).

   ```
   /usr/lib/postgresql/13/bin/pg_upgrade \
   --old-datadir=/var/lib/postgresql/9.6/main \
   --new-datadir=/var/lib/postgresql/13/main \
   --old-bindir=/usr/lib/postgresql/9.6/bin \
   --new-bindir=/usr/lib/postgresql/13/bin \
   --old-options '-c config_file=/etc/postgresql/9.6/main/postgresql.conf' \
   --new-options '-c config_file=/etc/postgresql/13/main/postgresql.conf' \
   --check
   ```

1. Migrate the data (without the `--check` argument).

   ```
   /usr/lib/postgresql/13/bin/pg_upgrade \
   --old-datadir=/var/lib/postgresql/9.6/main \
   --new-datadir=/var/lib/postgresql/13/main \
   --old-bindir=/usr/lib/postgresql/9.6/bin \
   --new-bindir=/usr/lib/postgresql/13/bin \
   --old-options '-c config_file=/etc/postgresql/9.6/main/postgresql.conf' \
   --new-options '-c config_file=/etc/postgresql/13/main/postgresql.conf'
   ```

1. Log out of the PostgreSQL user.

   ```bash
   exit
   ```

1. Swap the ports for the old and new PostgreSQL versions.

   ```
   $ sudo vim /etc/postgresql/13/main/postgresql.conf
   # change "port = 5433" to "port = 5432"

   $ sudo vim /etc/postgresql/9.6/main/postgresql.conf
   # change "port = 5432" to "port = 5433"
   ```

1. Start the PostgreSQL service.

   ```bash
   sudo systemctl start postgresql.service
   ```

1. Log in as the postgres user and confirm that the new PostgreSQL version is 13.3.

   ```
   $ sudo su postgres
   $ psql -c "SELECT version();"
                                                                      version
   ---------------------------------------------------------------------------------------------------------------------------------------------
    PostgreSQL 13.3 (Ubuntu 13.3-1.pgdg16.04+1) on x86_64-pc-linux-gnu, compiled by gcc (Ubuntu 5.4.0-6ubuntu1~16.04.12) 5.4.0 20160609, 64-bit
   (1 row)

   ```

1. Run `reindexdb`. Example:

   ```
   $ /usr/bin/reindexdb --all
   reindexdb: reindexing database "bifrost"
   reindexdb: reindexing database "oc_id"
   reindexdb: reindexing database "opscode_chef"
   reindexdb: reindexing database "postgres"
   reindexdb: reindexing database "template1"
   ```

1. Log into the Chef Infra Server machine.

1. Check the status of Chef Infra Server. PostgreSQL should be connected.

   ```
   $ sudo chef-server-ctl status
   -------------------
    Internal Services
   -------------------
   run: bookshelf: (pid 15763) 219163s; run: log: (pid 16559) 228464s
   run: elasticsearch: (pid 15797) 219162s; run: log: (pid 16345) 228507s
   run: nginx: (pid 15901) 219162s; run: log: (pid 16745) 228452s
   run: oc_bifrost: (pid 15909) 219161s; run: log: (pid 16237) 228519s
   run: oc_id: (pid 15915) 219161s; run: log: (pid 16255) 228513s
   run: opscode-erchef: (pid 15948) 219160s; run: log: (pid 16673) 228458s
   run: redis_lb: (pid 15952) 219160s; run: log: (pid 16779) 228445s
   -------------------
    External Services
   -------------------
   run: postgresql: connected OK to 10.0.11.0:5432
   ```

#### Upgrade Failure Troubleshooting

1. If the upgrade failed and you are left with a corrupted Chef Infra Server and/or a corrupted database, **DO NOT RISK YOUR BACKUP OF THE DATABASE.** Take all steps necessary to preserve the backup, including copying it to another disk. Consult with a professional sysadmin for instructions and best practices.

1. Contact customer support.

1. Reinstall the original version of Chef Infra Server you were using before attempting the upgrade process (if you had to perform a stepped upgrade, [install your original version of Chef Infra Server]({{< relref "install_server" >}}) before the stepped upgrade, not any versions you upgraded to in the stepped upgrade process). Again, **DO NOT RISK YOUR BACKUP OF THE DATABASE.** For example, consider using a separate disk from your backup for the new installation.

1. Consult the documentation on [knife-ec-restore](https://blog.chef.io/migrating-chef-server-knife-ec-backup-knife-tidy), and restore the database from the path where it was saved.

   Example:

   ```bash
   /opt/chefdk/embedded/bin/knife ec restore /backup/
   ```

1. Do not continue upgrading PostgreSQL until you have an uncorrupted Chef Infra Server and an uncorrupted PostgreSQL database.

### Chef Backend Install

{{% EOL_backend %}}

The Chef Infra Server can operate in a high availability configuration that provides automated load balancing and failover for stateful components in the system architecture.

To upgrade your Chef Backend installation, see [High Availability: Upgrade to Chef Backend 2]({{< relref "upgrade_server_ha_v2" >}}).

### Tiered Install

This section describes the upgrade process from a tiered server configuration.

{{< note >}}

These instructions are intended for users of the Chef Infra Server `tier` topology.
For the latest information on setting up a highly-available server cluster, see [High Availability: Backend Cluster]({{< relref "install_server_ha" >}}).

{{< /note >}}

### Tiered Upgrade Steps

To upgrade to Chef Infra Server on a tiered Chef Infra Server configuration, do the following:

1. Back up the Chef Infra Server data before starting the upgrade process using [knife-ec-backup](https://github.com/chef/knife-ec-backup).

2. Confirm that the Chef Infra Server services are operational:

    ```bash
    chef-server-ctl reconfigure
    ```

3. Download the desired Chef Infra Server version from the [Chef Infra Server Downloads](https://downloads.chef.io/products/chef-server) page, then copy it to each server.

4. Stop all front end servers:

    ```bash
    chef-server-ctl stop
    ```

5. Install the Chef Infra Server package on all servers:

    To install with `dpkg`:

    ```bash
    dpkg -i /path/to/chef-server-core-<version>.deb
    ```

    To install with the RPM Package Manager:

    ```bash
    rpm -Uvh --nopostun /path/to/chef-server-core-<version>.rpm
    ```

6. Stop the back end server:

    ```bash
    chef-server-ctl stop
    ```

7. Upgrade the server and accept the Chef Software license by entering `Yes` at the prompt:

    ```bash
    chef-server-ctl upgrade
    ```

    To accept the license and upgrade in one command:

    ```bash
    CHEF_LICENSE='accept' chef-server-ctl upgrade
    ```

8. Copy the entire `/etc/opscode` directory from the back end server to all front end servers:

    ```bash
    scp -r /etc/opscode <each server's IP>:/etc
    ```

9. Upgrade each of the front end servers:

    ```bash
    chef-server-ctl upgrade
    ```

10. Run the following command on both the front end, and back end servers:

    ```bash
    chef-server-ctl start
    ```

11. [Upgrade]({{< relref "#upgrading-manage-add-on" >}}) any Chef Infra Server add-ons.

12. After the upgrade process is complete, test and verify that the server works properly.

13. Clean up the server by removing the old data:

   ```bash
   chef-server-ctl cleanup
   ```

{{< note >}}

Check the [post upgrade steps](#post-upgrade-steps) if you are upgrading from a version before Chef Infra Server 14.8 to a version greater than or equal to 14.8.

{{</note >}}

### Upgrading Manage Add-On

Chef Manage is a management console for data bags, attributes, run-lists, roles, environments, and cookbooks from a web user interface.

Chef Infra Server 13 and 14 support the Chef Manage add-on. This add-on is [deprecated]({{< relref "versions#deprecated-products-and-versions" >}}) and will reach [EOL]({{< relref "versions#end-of-life-eol-products" >}}) on December 31, 2021. After upgrading Chef Infra Server, reinstall the add-on and then reconfigure Chef Infra Server and the add-on.

#### Use Downloads.chef.io

{{% ctl_chef_server_install_features_download %}}

#### Use Local Packages

{{% ctl_chef_server_install_features_manual %}}
