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

Each new release of Chef Infra Server improves reliability and updates 3rd party components to ensure the security of the server. It is important to keep Chef Infra Server up to date to ensure the secure and reliable operation of Chef Infra in your organization.

{{< warning >}}
Before upgrading a production server make sure to upgrade a test server to confirm the process.
{{< /warning >}}

## Upgrade Matrix

If running a Chef Infra Server 12.17.15 or later you can upgrade directly to the latest releases of Chef Infra Server. If you are running a release before 12.17.15 you must perform a stepped upgrade as outlined below.

| Running Version | Upgrade To Version | Requires License | Supported Version |
|---------|---------|------|-------------------|
| 14 | 15 | Yes | Yes |
| 13 | 15 | Yes | No |
| 12.17.15 | 15 | Yes | No |
| 12.3.0 | 12.17.15 | No | No |
| 11 | 12.3.0 | No | No |

Requires License
: Chef Infra Server 13 and later are governed by the [Chef EULA]({{< relref "chef_license" >}}). You are required to accept these terms when using Chef Infra Server for the first time by entering `Yes` when prompted.

Supported Release
: Chef Infra Server 14 and later are supported Chef Software releases. Earlier releases are not supported. For more information about supported Chef Software see the [Supported Versions]({{< relref "/versions#supported-commercial-distributions" >}}) documentation.

## General Chef Infra Server Upgrade Process

### Standalone Server

{{< note >}}

See the [Release-Specific Steps](#release-specific-steps) for information about upgrading specific versions of Chef Infra Server **before** starting the upgrade process.

{{< /note >}}

{{% chef-server/server_upgrade_duration %}}

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

1. Download the desired Chef Infra Server version from the [Chef Infra Server Downloads](https://www.chef.io/downloads).

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
   reindexdb: reindexing database "oc_id"
   reindexdb: reindexing database "opscode-pgsql"
   reindexdb: reindexing database "opscode_chef"
   reindexdb: reindexing database "postgres"
   reindexdb: reindexing database "template1"
   ```

{{% chef-server/server_analyze_postgresql_db %}}

You are now finished with the upgrade.

#### Upgrade Failure Troubleshooting

1. If the upgrade failed and you have a corrupted Chef Infra Server and/or a corrupted database, **DO NOT RISK YOUR BACKUP OF THE DATABASE.** Take all steps necessary to preserve the backup, including copying it to another disk. Consult with a professional sysadmin for instructions and best practices.

1. Contact customer support.

1. Reinstall the original version of Chef Infra Server you were using before attempting the upgrade process (if you had to perform a stepped upgrade, [install your original version of Chef Infra Server]({{< relref "install_server" >}}) before the stepped upgrade, not any versions you upgraded to in the stepped upgrade process). Again, **DO NOT RISK YOUR BACKUP OF THE DATABASE.** For example, consider using a separate disk from your backup for the new installation.

1. Consult the [restore documentation]({{< relref "server_backup_restore" >}}) and restore the database from the path to where it was saved:

   ```bash
   chef-server-ctl restore /path/to/tar/archive.tar.gz
   ```

### External PostgreSQL

The following External PostgreSQL upgrade steps are provided as a courtesy only.  It is the responsibility of the user to upgrade and maintain any External PostgreSQL configurations.

#### Upgrade Chef Infra Server

1. Log into the external PostgreSQL machine.

1. Run `vacuumdb` before starting the upgrade:

   ```bash
   sudo su postgres
   /usr/bin/vacuumdb --all --full
   exit
   ```

1. Log into the Chef Infra Server machine.

1. Consult the documentation on [`knife-ec-backup`](https://blog.chef.io/migrating-chef-server-knife-ec-backup-knife-tidy).

   Install `knife-ec-backup`, if it not already installed. A sample session follows
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

   If you are running a Chef Infra Server release before 12.17.15, you cannot upgrade directly to the latest. You must perform a stepped upgrade first.

   - If you are running Chef Infra Server 12.3.0, upgrade to 12.17.15.
   - If you are running Chef Infra Server 11, you must first upgrade to 12.3.0, and then to 12.17.15.

   After performing the stepped upgrade, return here and continue with the next step below.

1. [Download](https://www.chef.io/downloads) the desired version of Chef Infra Server.

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
   rpm -Uvh --nopostun --force /path/to/chef-server-core-VERSION.rpm
   ```

1. Upgrade Chef Infra Server and accept the Chef Software license by entering `Yes` at the prompt:

   ```bash
   chef-server-ctl upgrade
   ```

   To accept the license and upgrade in one command:

   ```bash
   CHEF_LICENSE='accept' chef-server-ctl upgrade
   ```

1. If the upgrade failed, see the section on [upgrade failure troubleshooting](#upgrade-failure-troubleshooting-1).

   If the upgrade was successful, start the Chef Infra Server services and cleanup.

   ```bash
   sudo chef-server-ctl start
   sudo chef-server-ctl cleanup
   ```

   You are now finished with the Chef Infra Server upgrade. Proceed directly to the **Upgrade PostgreSQL** section.

#### Upgrade PostgreSQL

1. Log into the external PostgreSQL machine.

1. Update packages and install your selected PostgreSQL version.
   Example (Ubuntu/PostgreSQL 13.3):

   ```bash
   sudo apt-get update
   sudo apt-get install postgresql-13
   ```

1. Check if there are any differences in the config files. Make sure to update the new config files if required.
   Example (PostgreSQL 13.3):

   ```bash
   diff /etc/postgresql/OLD_POSTGRESQL_VERSION/main/postgresql.conf /etc/postgresql/13/main/postgresql.conf
   diff /etc/postgresql/OLD_POSTGRESQL_VERSION/main/pg_hba.conf     /etc/postgresql/13/main/pg_hba.conf
   ```

1. Stop the PostgreSQL service.

   ```bash
   sudo systemctl stop postgresql.service
   ```

1. Log in as the `postgres` user.

   ```bash
   su postgres
   ```

1. Ensure that you are in a directory where you can run the `pg_upgrade` command.

   Example:

   ```bash
   cd /tmp
   ```

1. Check clusters (notice the `--check` argument, this will not change any data).
   Example (PostgreSQL 13.3):

   ```bash
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
   Example (PostgreSQL 13.3):

   ```bash
   /usr/lib/postgresql/13/bin/pg_upgrade \
   --old-datadir=/var/lib/postgresql/9.6/main \
   --new-datadir=/var/lib/postgresql/13/main \
   --old-bindir=/usr/lib/postgresql/9.6/bin \
   --new-bindir=/usr/lib/postgresql/13/bin \
   --old-options '-c config_file=/etc/postgresql/9.6/main/postgresql.conf' \
   --new-options '-c config_file=/etc/postgresql/13/main/postgresql.conf'
   ```

1. Log out of the `postgres` user.

   ```bash
   exit
   ```

1. Swap the ports for the old and new PostgreSQL versions.
   Example (PostgreSQL 13.3):

   ```bash
   $ sudo vim /etc/postgresql/13/main/postgresql.conf
   # change "port = 5433" to "port = 5432"

   $ sudo vim /etc/postgresql/9.6/main/postgresql.conf
   # change "port = 5432" to "port = 5433"
   ```

1. Start the PostgreSQL service.

   ```bash
   sudo systemctl start postgresql.service
   ```

1. Log in as the `postgres` user and confirm that the new PostgreSQL version is correct.
   Example (PostgreSQL 13.3):

   ```bash
   $ sudo su - postgres
   $ psql -c "SELECT version();"
                                                                      version
   ---------------------------------------------------------------------------------------------------------------------------------------------
    PostgreSQL 13.3 (Ubuntu 13.3-1.pgdg16.04+1) on x86_64-pc-linux-gnu, compiled by gcc (Ubuntu 5.4.0-6ubuntu1~16.04.12) 5.4.0 20160609, 64-bit
   (1 row)

   ```

1. Run `reindexdb`. Example:

   ```bash
   $ /usr/bin/reindexdb --all
   reindexdb: reindexing database "bifrost"
   reindexdb: reindexing database "oc_id"
   reindexdb: reindexing database "opscode_chef"
   reindexdb: reindexing database "postgres"
   reindexdb: reindexing database "template1"
   ```

{{% chef-server/server_analyze_postgresql_db %}}

1. Log into the Chef Infra Server machine.

1. Check the status of Chef Infra Server. PostgreSQL should be connected.

   ```bash
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

{{% chef-server/EOL_backend %}}

The Chef Infra Server can operate in a high availability configuration that provides automated load balancing and failover for stateful components in the system architecture.

To upgrade your Chef Backend installation, see [High Availability: Upgrade to Chef Backend 2]({{< relref "upgrade_server_ha_v2" >}}).

### Tiered Install

This section describes the upgrade process from a tiered server configuration.

{{< note >}}

These instructions are for the Chef Infra Server `tier` topology.
For the latest information on setting up a highly available server cluster, see [High Availability: Backend Cluster]({{< relref "install_server_ha" >}}).

{{< /note >}}

### Tiered Upgrade Steps

To upgrade to Chef Infra Server on a tiered Chef Infra Server configuration, do the following:

1. Back up the Chef Infra Server data before starting the upgrade process using [knife-ec-backup](https://github.com/chef/knife-ec-backup).

2. Confirm that the Chef Infra Server services are operational:

    ```bash
    chef-server-ctl reconfigure
    ```

3. Download the desired Chef Infra Server version from [Chef Downloads](https://www.chef.io/downloads), then copy it to each server.

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
   
9. Run the following command on the back end servers:

    ```bash
    chef-server-ctl start
    ```

10. Upgrade each of the front end servers:

    ```bash
    chef-server-ctl upgrade
    ```

11. Run the following command on both the front end:

    ```bash
    chef-server-ctl start
    ```

12. [Upgrade]({{< relref "#upgrading-manage-add-on" >}}) any Chef Infra Server add-ons.

13. After the upgrade process is complete, test and verify that the server works.

14. Clean up the server by removing the old data:

```bash
chef-server-ctl cleanup
```

## Release-Specific Steps

### Upgrading to 15.5 or later (tiered installations only)

The Chef Infra Server 15.5 upgrade from 15.0.X or later does not automatically reindex for  Tiered installations.
{{% chef-server/server_upgrade_duration %}}

#### steps for reindex
1. Run the below command on frontend server's
```bash
chef-server-ctl reindex
```

{{< note >}}

`chef-server-ctl reindex` is a downtime operation.

{{</note >}}

Chef Infra Server 15.5 is the minimum recommended version for upgrade from older versions lessthan 15 for tiered installations.

### Upgrading to 15.x

Chef Infra Server 15.0 moved from Elasticsearch to OpenSearch as its search index.

{{% chef-server/server_upgrade_duration %}}

The Chef Infra Server 15 will automatically transfer search data from Elasticsearch to OpenSearch without the need for a reindex. The Chef Infra Server 15 upgrade will need to manually reindex existing external Elasticsearch installations.

The upgrade duration might take more time if you are upgrading from Chef Infra Server 12.x/13.x, as it automatically reindexes your database.

### Upgrading to 14.x

Chef Infra Server 14.0 moved from Solr to Elasticsearch as its search index.
{{% chef-server/server_upgrade_duration %}}

The Chef Infra Server 14 upgrade does not automatically reindex existing external Elasticsearch installations.

#### Upgrading to 14.14

Chef Infra Server 14.14 supports external OpenSearch for indexing. Please follow the migration section below to migrate from Elasticsearch to external OpenSearch.

#### Steps To Enable External OpenSearch

1. Set the `elasticsearch['enable']` attribute to `false`.
1. Set the `opensearch['external']` attribute to `true`.
1. Set the `opensearch['external_url']` attribute to the external OpenSearch URL.
1. Set the `opscode_erchef['search_queue_mode']` attribute to `batch`.
1. Set the `opscode_erchef['search_provider']` attribute to `opensearch`.
1. Set the `opscode_erchef['search_auth_username']` attribute to OpenSearch username.
1. Set the `opscode_erchef['search_auth_password']` attribute to OpenSearch password.

For example:

```bash
elasticsearch['enable'] = false
opscode_erchef['search_queue_mode'] = 'batch'
opscode_erchef['search_provider'] = 'opensearch'
opensearch['external'] = true
opensearch['external_url'] = "http://127.0.0.1:9200"
opscode_erchef['search_auth_username'] = "OPEN_SEARCH_USER"
opscode_erchef['search_auth_password'] = "OPEN_SEARCH_PWD"
```

{{< note >}}

The OpenSearch user should have full access to the cluster, including access to all cluster-wide operations and the ability to write to all indices. We recommend that the user has the admin backend role. 

Please refer to OpenSearch's documentation on [predefined roles](https://opensearch.org/docs/latest/security-plugin/access-control/users-roles/#predefined-roles) and [role mapping configuration](https://opensearch.org/docs/latest/security-plugin/configuration/yaml#roles_mappingyml).

This user must be created on the external OpenSearch cluster. The Chef Infra Server executable cannot be used to create this user on external OpenSearch setups.
{{</note >}}

#### Steps To Migrate from Elasticsearch to External OpenSearch

There are two ways to migrate from Elasticsearch to external OpenSearch: migrating your data, or reindexing and reconfiguring your database.

We recommend migrating your data over reindexing and reconfiguring.

**Migrate Data**

Copy or move your Elasticsearch OSS data and logs directories to the newly installed OpenSearch paths. See OpenSearch's [documentation on upgrading to OpenSearch](https://opensearch.org/docs/latest/upgrade-to/upgrade-to/#upgrade-to-opensearch). 

**Reindex and Reconfigure**

Reindex and reconfigure your database after upgrading to Chef Infra Server 14.13. The duration of this operation will vary depending on your server hardware and the number of node objects on your Chef Infra Server. 

Use the Chef Infra Server command-line tool to reindex and reconfigure your database:

```bash
chef-server-ctl reindex
chef-server-ctl reconfigure
```

#### Upgrading to 14.8

Chef Infra Server 14.8 upgrades PostgreSQL from 9.6 to 13.3. The 14.8 upgrade process requires a one-time downtime to vacuum, upgrade, and re-index the database. The entire upgrade operation takes about one minute for each 1000 nodes (1000 nodes is approximately 286MB). This process may take longer depending on your server hardware and the size of the node objects on your Chef Infra Server.

{{< note >}}

Set the `postgresql['pg_upgrade_timeout']` attribute in [chef-server.rb]({{< relref "config_rb_server_optional_settings" >}}) to the timeout value for the upgrade. Set this value based on the size of your data, where it take about one minute per 1,000 nodes which is approximately 286MB.

{{</note >}}

#### Upgrading to 14.16

Chef Infra Server 14.16 includes a bug fix for the bifrost database. This bug may create unused authorization IDs in the bifrost database. After upgrading to Chef Infra Server 14.16, the unused authorization IDs must be manually deleted.

To analyze the data that gets deleted and get an estimate of the time needed to delete the data, run:

```bash
chef-server-ctl cleanup-bifrost --estimate-only
```

To delete the unused authorization IDs from the bifrost database, run:


```bash
chef-server-ctl cleanup-bifrost
```

##### Database Preparation

1. Run `VACUUM FULL` on the PostgreSQL database if you don't have automatic vacuuming set up. This process will reduce the size of the database by deleting unnecessary data and speeds up the migration. The `VACUUM FULL` operation takes around 1 to 2 minutes per gigabyte of data depending on the complexity of the data, and requires free disk space at least as large as the size of your database.

    ```bash
       sudo su - opscode-pgsql
       /opt/opscode/embedded/bin/vacuumdb --all --full
    ```

   You should then see output like:

    ```bash
       vacuumdb: vacuuming database "bifrost"
       vacuumdb: vacuuming database "oc_id"
       vacuumdb: vacuuming database "opscode-pgsql"
       vacuumdb: vacuuming database "opscode_chef"
       vacuumdb: vacuuming database "postgres"
       vacuumdb: vacuuming database "template1"
    ```

{{% chef-server/server_analyze_postgresql_db %}}

1. Back up the PostgreSQL database before upgrading so you can restore the full database to a previous release in the event of a failure. See [Backup and Restore]({{< relref "server_backup_restore" >}}) for more information.

##### Upgrade Steps

Follow the Chef Infra Server upgrade instructions below.

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
