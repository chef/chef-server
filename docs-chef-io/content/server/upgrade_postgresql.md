+++
title = "Upgrade Postgresql"

date = 2022-02-04T15:30:41-08:00
draft = false

[menu]
  [menu.server]
    title = "Upgrade Postgresql"
    identifier = "server/upgrade/upgrade_postgresql"
    parent = "server/upgrade"
    weight = 40
+++

The following External PostgreSQL upgrade steps are provided as a courtesy only.  It is the responsibility of the user to upgrade and maintain any External PostgreSQL configurations.

## Upgrade to 14.8

Chef Infra Server 14.8 upgrades PostgreSQL from 9.6 to 13.3. The 14.8 upgrade process requires a one-time downtime to vacuum, upgrade, and re-index the database. The entire upgrade operation takes about one minute for each 1000 nodes (1000 nodes is approximately 286MB). This process may take longer depending on your server hardware and the size of the node objects on your Chef Infra Server.

{{< note >}}

Set the `postgresql['pg_upgrade_timeout']` attribute in [chef-server.rb]({{< relref "config_rb_server_optional_settings" >}}) to the timeout value for the upgrade. Set this value based on the size of your data, where it take about one minute per 1,000 nodes which is approximately 286MB.

{{</note >}}

### Database Preparation

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

1. Back up the PostgreSQL database before upgrading so you can restore the full database to a previous release in the event of a failure. See [Backup and Restore]({{< relref "server_backup_restore" >}}) for more information.

### Upgrade Steps

Follow the Chef Infra Server upgrade instructions below.

{{< note >}}

Estimates are based on an 8-core 32 GB memory (t3.2xlarge) AWS EC2 instance with 2 organizations and 3 users having 565,000+ nodes, 13,000+ cookbooks, 29,000+ databags, 274,000+ environments, and 281,000+ roles.

{{</note >}}

## Upgrade Chef Infra Server

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

   If you are running a Chef Infra Server release before 12.17.15, you cannot upgrade directly to 14.8.X. You must perform a stepped upgrade first.

   - If you are running Chef Infra Server 12.3.0, upgrade to 12.17.15.
   - If you are running Chef Infra Server 11, you must first upgrade to 12.3.0, and then to 12.17.15.

   After performing the stepped upgrade, return here and continue with the next step below.

1. [Download](https://www.chef.io/downloads/tools/infra-server) the desired version of Chef Infra Server.

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

## Upgrade PostgreSQL

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

## Upgrade Failure Troubleshooting

1. If the upgrade failed and you are left with a corrupted Chef Infra Server and/or a corrupted database, **DO NOT RISK YOUR BACKUP OF THE DATABASE.** Take all steps necessary to preserve the backup, including copying it to another disk. Consult with a professional sysadmin for instructions and best practices.

1. Contact customer support.

1. Reinstall the original version of Chef Infra Server you were using before attempting the upgrade process (if you had to perform a stepped upgrade, [install your original version of Chef Infra Server]({{< relref "install_server" >}}) before the stepped upgrade, not any versions you upgraded to in the stepped upgrade process). Again, **DO NOT RISK YOUR BACKUP OF THE DATABASE.** For example, consider using a separate disk from your backup for the new installation.

1. Consult the documentation on [knife-ec-restore](https://blog.chef.io/migrating-chef-server-knife-ec-backup-knife-tidy), and restore the database from the path where it was saved.

   Example:

   ```bash
   /opt/chefdk/embedded/bin/knife ec restore /backup/
   ```

1. Do not continue upgrading PostgreSQL until you have an uncorrupted Chef Infra Server and an uncorrupted PostgreSQL database.
