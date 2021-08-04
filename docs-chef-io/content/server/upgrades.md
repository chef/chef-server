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

If running a Chef Infra Server 12.17.15 or later you can upgrade directly to the latest releases of Chef Infra Server 14. If you are running a release prior to 12.17.15 you must perform a stepped upgrade as outlined below.

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

### Upgrading to 14.x

Chef Infra Server 14 moved from Solr to Elasticsearch as its search index.
{{% server_upgrade_duration %}}

The Chef Infra Server 14 upgrade does not automatically reindex existing external Elasticsearch installations.

### Upgrading to 14.8.x

Chef Infra Server 14.8 upgraded postgresql from 9.6 to 13.3. There are a couple of extra steps that need to be considered depending on the scenario.

#### Upgrading steps

1. Follow all the instructions for upgrading to 14

2. Reindex the Chef Infra server

    ```bash
    /usr/lib/postgresql/13.3/bin/reindexdb --all
    ```

#### Recommendations

1. Running Vacuum full is recommended if auto vacuuming is not set up, as this will reduce the size of the db by deleting the unwanted data. We estimate the vacuum full operation will take around 1 to 2 minutes per GB of data and it will also be dependent on the nature of the data also.

    ```bash
    /usr/lib/postgresql/9.6/bin/vaccumedb --all --full
    ```

2. Taking a chef-server-ctl backup or knife-ec-backup is recommended before starting the upgrade. The backup takes around 4 to 5 minutes per GB of data.

#### Additional Information

1. The estimations were done on the machine with 8 cores with 32 GB memory (t3.2xlarge EC2 machine).

2. The down time for standalone scenario upgrade with 158G of unvacuumed data was about 5hr 15min.

### Upgrading to 12.17.15

{{< warning >}}
Upgrade Chef Infra Server and any add-ons to compatible versions before setting `insecure_addon_compat` to `false`.
{{< /warning >}}

As of version 12.14, Chef Infra Server renders passwords inside of the `/etc/opscode` directory by default.
If you are using Chef Infra Server without add-ons, or if you are using the latest add-ons versions, you can set `insecure_addon_compat` to `false` in `/etc/opscode/chef-server.rb.
and Chef Infra Server will write all credentials to a single location.

For more information on password generation, including a list of supported add-on versions, see [Chef Infra Server Credentials Management]({{< relref "server_security/#chef-infra-server-credentials-management" >}}).

### Upgrading to 12.3.0

If you are running a Chef Infra Server release prior to 12.3.0 please contact Chef Support for additional guidance on upgrading your Chef Infra Server installation.

## Chef Infra Server 14 Upgrade Process

### Standalone Server

{{% server_upgrade_duration %}}

#### Standalone Upgrade Steps

1. Back up your Chef Infra Server data before starting the upgrade process using [knife-ec-backup](https://github.com/chef/knife-ec-backup).

2. Confirm that the Chef Infra Server services are operational:

    ```bash
    chef-server-ctl reconfigure
    ```

3. Download the desired Chef Infra Server version from the [Chef Infra Server Downloads](https://downloads.chef.io/products/chef-server) page.

4. Stop the server:

    ```bash
    chef-server-ctl stop
    ```

5. Install the Chef Infra Server package:

    To install with `dpkg`:

    ```bash
    dpkg -i /path/to/chef-server-core-<version>.deb
    ```

    To install with the RPM Package Manager:

    ```bash
    rpm -Uvh --nopostun /path/to/chef-server-core-<version>.rpm
    ```

6. Upgrade the server and accept the Chef Software license by entering `Yes` at the prompt:

    ```bash
    chef-server-ctl upgrade
    ```

    To accept the license and upgrade in one command:

    ```bash
    CHEF_LICENSE='accept' chef-server-ctl upgrade
    ```

7. Start Chef Infra Server:

    ```bash
    chef-server-ctl start
    ```

8. [Upgrade](#upgrading-add-ons) any Chef Infra Server add-ons.

9. After the upgrade process is complete, test and verify that the server works properly.

10. Clean up the server by removing the old data:

    ```bash
    chef-server-ctl cleanup
    ```

### Chef Backend Install

{{% EOL_backend %}}

The Chef Infra Server can operate in a high availability configuration that provides automated load balancing and failover for stateful components in the system architecture.

To upgrade your Chef Backend installation, see [High Availability: Upgrade to Chef Backend 2]({{< relref "upgrade_server_ha_v2" >}}).

### Tiered Install

This section describes the upgrade process from a tiered server configuration.

{{% server_upgrade_duration %}}

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

11. [Upgrade]({{< relref "#upgrading-add-ons" >}}) any Chef Infra Server add-ons.

12. After the upgrade process is complete, test and verify that the server works properly.

13. Clean up the server by removing the old data:

   ```bash
   chef-server-ctl cleanup
   ```

### Upgrading Manage Add-On

Chef Manage is a management console for data bags, attributes, run-lists, roles, environments, and cookbooks from a web user interface.

Chef Infra Server 13 and 14 support the Chef Manage add-on. This add-on is [deprecated]({{< relref "versions#deprecated-products-and-versions" >}}) and will reach [EOL]({{< relref "versions#end-of-life-eol-products" >}}) on December 31, 2021. After upgrading Chef Infra Server, reinstall the add-on and then reconfigure Chef Infra Server and the add-on.

#### Use Downloads.chef.io

{{% ctl_chef_server_install_features_download %}}

#### Use Local Packages

{{% ctl_chef_server_install_features_manual %}}


### External postgres upgrade

In the external postgres setup, we will be having the postgress install outside the Chef Infra server. Upgradeing the Chef Infra server to 14.8 or later reguires to migrate the external postgres from 9.6 to 13.3 seperately.

#### Recommended steps for the external postgres upgrade


1. Back up your Chef Infra Server data before starting the upgrade process using [knife-ec-backup](https://github.com/chef/knife-ec-backup).

2. Confirm that the Chef Infra Server services are operational:

    ```bash
    chef-server-ctl reconfigure
    ```

3. Download the desired Chef Infra Server version from the [Chef Infra Server Downloads](https://downloads.chef.io/products/chef-server) page.

4. Stop the server:

    ```bash
    chef-server-ctl stop
    ```

5. Install the Chef Infra Server package:

    To install with `dpkg`:

    ```bash
    dpkg -i /path/to/chef-server-core-<version>.deb
    ```

    To install with the RPM Package Manager:

    ```bash
    rpm -Uvh --nopostun /path/to/chef-server-core-<version>.rpm
    ```

6. Upgrade the server and accept the Chef Software license by entering `Yes` at the prompt:

    ```bash
    chef-server-ctl upgrade
    ```

    To accept the license and upgrade in one command:

    ```bash
    CHEF_LICENSE='accept' chef-server-ctl upgrade
    ```

7. Start Chef Infra Server:

    ```bash
    chef-server-ctl start
    ```

8. Update packages and install the new PostgreSQL 13.3 on the external postgres machine.

9. Check if there are any differences in the config files.Make sure to update them if required

    ```bash
    diff /etc/postgresql/12/main/postgresql.conf /etc/postgresql/13/main/postgresql.conf
    diff /etc/postgresql/12/main/pg_hba.conf /etc/postgresql/13/main/pg_hba.conf
    ```

10. Stop the PostgreSQL service.

    ```bash
    sudo systemctl stop postgresql.service
    ```

11. Logging in as postgres user, check clusters (notice the --check argument, this will not change any data).

    ```bash
    su postgres
    /usr/lib/postgresql/13/bin/pg_upgrade \
      --old-datadir=/var/lib/postgresql/9.6/main \
      --new-datadir=/var/lib/postgresql/13/main \
      --old-bindir=/usr/lib/postgresql/9.6/bin \
      --new-bindir=/usr/lib/postgresql/13/bin \
      --old-options '-c config_file=/etc/postgresql/9.6/main/postgresql.conf' \
      --new-options '-c config_file=/etc/postgresql/13/main/postgresql.conf' \
      --check
    ```

12. Migrate the data and logout of postgres user(without the --check argument).

    ```bash
    /usr/lib/postgresql/13/bin/pg_upgrade \
      --old-datadir=/var/lib/postgresql/9.6/main \
      --new-datadir=/var/lib/postgresql/13/main \
      --old-bindir=/usr/lib/postgresql/9.6/bin \
      --new-bindir=/usr/lib/postgresql/13/bin \
      --old-options '-c config_file=/etc/postgresql/9.6/main/postgresql.conf' \
      --new-options '-c config_file=/etc/postgresql/13/main/postgresql.conf'
    exit
    ```
    
13. Swap the ports for the old and new PostgreSQL versions.

    ```bash
    sudo vim /etc/postgresql/13/main/postgresql.conf
    # change "port = 5433" to "port = 5432"

    sudo vim /etc/postgresql/9.6/main/postgresql.conf
    # change "port = 5432" to "port = 5433"
    ```
14. Start the PostgreSQL service.

    ```bash
    sudo systemctl start postgresql.service
    ```

15. Log in as the postgres user again and Check the new PostgreSQL version.

    ```bash
    sudo su postgres
    psql -c "SELECT version();"
    ```

16. Check the status in the Chef Infra server machine

    ```bash
    chef-server-ctl status
    ```
