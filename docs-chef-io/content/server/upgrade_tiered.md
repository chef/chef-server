+++
title = "Tiered"

date = 2022-02-04T15:38:16-08:00
draft = false

[menu]
  [menu.server]
    title = "Upgrade Tiered"
    identifier = "server/upgrade/upgrade_tiered"
    parent = "server/upgrade"
    weight = 70
+++

To upgrade to Chef Infra Server on a tiered Chef Infra Server configuration, do the following:

1. Back up the Chef Infra Server data before starting the upgrade process using [knife-ec-backup](https://github.com/chef/knife-ec-backup).

2. Confirm that the Chef Infra Server services are operational:

    ```bash
    chef-server-ctl reconfigure
    ```

3. Download the desired Chef Infra Server version from the [Chef Infra Server Downloads](https://www.chef.io/downloads/tools/infra-server) page, then copy it to each server.

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

12. After the upgrade process is complete, test and verify that the server works.

13. Clean up the server by removing the old data:

   ```bash
   chef-server-ctl cleanup
   ```
