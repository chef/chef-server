+++
title = "Upgrade Chef Infra Server"
draft = false

gh_repo = "chef-server"

aliases = ["/upgrade_server.html", "/upgrade_server/"]

[menu]
  [menu.server]
    title = "Upgrades"
    identifier = "server/installation/Upgrades"
    parent = "server/installation"
    weight = 60
+++

## Chef Infra Server Upgrade Matrix

| Running Version | Upgrade Version | License | Version Support |
|---------|---------|------|-----------|
| 13 | 14 | Yes | Yes |
| 12.17.15 | 14 | Yes | Yes |
| 12.3.0 | 12.17.15 | No | No |
| 11 | 12.3.0 | No | No |

License
: Chef Infra Server 13 and 14 are governed by the [Chef License](https://docs.chef.io/chef_license_accept/#chef-infra-server). You will be required to accept these terms when using Chef Infra Server 13 or 14 for the first time by entering `Yes` when prompted.

Version Support
: Chef Infra Server 13 and 14 are supported Chef Software distributions. Earlier versions are no longer supported. For more information about supported Chef Software see the [Supported Versions](https://docs.chef.io/versions/#supported-commercial-distributions) documentation.

## Upgrading to Chef Infra Server

Three upgrade scenarios exist for upgrades from Chef Infra Server 12.17.15 to Chef Infra Server 13 or 14:

- [Standalone](/upgrade_server/#standalone-upgrade)
- [Chef Backend (HA)](/upgrade_server/#chef-backend-upgrade)
- [Tiered](/upgrade_server/#tiered-upgrade)

### Chef Infra Server 14

{{< warning >}}

Do not upgrade your production server. First, upgrade in your test server, and then upgrade your production server.

{{< /warning >}}

Chef Infra Server 14 uses Elasticsearch as its search index.
{{% server_upgrade_duration %}}

The Chef Infra Server 14 upgrade does not reindex existing external Elasticsearch installations.

### Chef Infra Server 12.17.15 or Later

{{< warning >}}
Upgrade Chef Infra Server and any add-ons to compatible versions before setting `insecure_addon_compat` to `false`.
{{< /warning >}}

As of version 12.14, Chef Infra Server renders passwords inside of the `/etc/opscode` directory by default.
If you are using Chef Infra Server without add-ons, or if you are using the latest add-ons versions, you can set `insecure_addon_compat` to `false` in `/etc/opscode/chef-server.rb.
and Chef Infra Server will write all credentials to a single location.

For more information on password generation, including a list of supported add-on versions, see [Chef Infra Server Credentials Management](/server_security/#chef-infra-server-credentials-management).

## Standalone Upgrade

{{% server_upgrade_duration %}}

### Standalone Upgrade Steps

1. Back up your Chef Infra Server data before starting the upgrade process using [knife-ec-backup](https://github.com/chef/knife-ec-backup).

1. Confirm that the Chef Infra Server services are operational:

    ```bash
    chef-server-ctl reconfigure
    ```

1. Download the desired Chef Infra Server version from the [Chef Infra Server Downloads](https://downloads.chef.io/products/chef-server) page.

1. Stop the server:

    ```bash
    chef-server-ctl stop
    ```

1. Install the Chef Infra Server package:

    To install with `dpkg`:

    ```bash
    dpkg -i /path/to/chef-server-core-<version>.deb
    ```

    To install with the RPM Package Manager:

    ```bash
    rpm -Uvh --nopostun /path/to/chef-server-core-<version>.rpm
    ```

1. Upgrade the server and accept the Chef Software license by entering `Yes` at the prompt:

    ```bash
    chef-server-ctl upgrade
    ```

    To accept the license and upgrade in one command:

    ```bash
    CHEF_LICENSE='accept' chef-server-ctl upgrade
    ```

1. Start Chef Infra Server:

    ```bash
    chef-server-ctl start
    ```

1. [Upgrade](#upgrading-add-ons) any Chef Infra Server add-ons.

1. After the upgrade process is complete, test and verify that the server works properly.

1. Clean up the server by removing the old data:

    ```bash
    chef-server-ctl cleanup
    ```

## Chef Backend Upgrade

The Chef Infra Server can operate in a high availability configuration that provides automated load balancing and failover for stateful components in the system architecture.

To upgrade your Chef Backend installation, see [High Availability: Upgrade to Chef Backend 2](/upgrade_server_ha_v2/).

## Tiered Upgrade

This section describes the upgrade process from a tiered server configuration.

{{% server_upgrade_duration %}}

{{< note >}}

These instructions are intended for users of the Chef Infra Server `tier` topology.
For the latest information on setting up a highly-available server cluster, see [High Availability: Backend Cluster](/install_server_ha/).

{{< /note >}}

## Tiered Upgrade Steps

To upgrade to Chef Infra Server on a tiered Chef Infra Server configuration, do the following:

1. Back up the Chef Infra Server data before starting the upgrade process using [knife-ec-backup](https://github.com/chef/knife-ec-backup).

1. Confirm that the Chef Infra Server services are operational:

    ```bash
    chef-server-ctl reconfigure
    ```

1. Download the desired Chef Infra Server version from the [Chef Infra Server Downloads](https://downloads.chef.io/products/chef-server) page, then copy it to each server.

1. Stop all front end servers:

    ```bash
    chef-server-ctl stop
    ```

1. Install the Chef Infra Server package on all servers:

    To install with `dpkg`:

    ```bash
    dpkg -i /path/to/chef-server-core-<version>.deb
    ```

    To install with the RPM Package Manager:

    ```bash
    rpm -Uvh --nopostun /path/to/chef-server-core-<version>.rpm
    ```

1. Stop the back end server:

    ```bash
    chef-server-ctl stop
    ```

1. Upgrade the server and accept the Chef Software license by entering `Yes` at the prompt:

    ```bash
    chef-server-ctl upgrade
    ```

    To accept the license and upgrade in one command:

    ```bash
    CHEF_LICENSE='accept' chef-server-ctl upgrade
    ```

1. Copy the entire `/etc/opscode` directory from the back end server to all front end servers:

    ```bash
    scp -r /etc/opscode <each server's IP>:/etc
    ```

1. Upgrade each of the front end servers:

    ```bash
    chef-server-ctl upgrade
    ```

1. Run the following command on both the front end, and back end servers:

    ```bash
    chef-server-ctl start
    ```

1. [Upgrade](#upgrading-add-ons) any Chef Infra Server add-ons.

1. After the upgrade process is complete, test and verify that the server works properly.

1. Clean up the server by removing the old data:

   ```bash
   chef-server-ctl cleanup
   ```

## Upgrading Add-ons

Chef Infra Server 13 and 14 supports Chef Manage and Push Jobs. Both of these add-ons are [deprecated](https://docs.chef.io/versions/#deprecated-products-and-versions). Push Jobs will reach EOL on December 31, 2020 and Chef Manage will reach EOL on December 31, 2021. After upgrading Chef Infra Server, reinstall the add-on and then reconfigure Chef Infra Server and the add-on.

Chef Manage
: Chef Manage is deprecated and will reach [EOL](https://docs.chef.io/versions/#deprecated-products-and-versions) on December 31, 2021. Chef Manage is a management console for  data bags, attributes, run-lists, roles, environments, and cookbooks from a web user interface

Push Jobs
: Push Jobs deprecated and will reach [EOL](https://docs.chef.io/versions/#deprecated-products-and-versions) on December 31, 2020. Chef Push Jobs is an extension of the Chef Infra Server that allows for running jobs against nodes independently of a Chef Infra Client run.

### Use Downloads

{{% ctl_chef_server_install_features_download %}}

### Use Local Packages

{{% ctl_chef_server_install_features_manual %}}
