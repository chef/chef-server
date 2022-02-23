+++
title = "High Availability: Upgrade to Chef Backend 2"
draft = false

gh_repo = "chef-server"

aliases = ["/upgrade_server_ha_v2.html", "/upgrade_server_ha_v2/"]

[menu]
  [menu.server]
    title = "Upgrade HA Cluster"
    description = "How to upgrade a Chef Backend (HA) Chef Infra Server cluster"
    identifier = "server/installation/Upgrade HA Cluster"
    parent = "server/installation"
    weight = 70
+++

{{% EOL_backend %}}

This documentation describes the process for upgrading a high availability Chef Infra Server cluster.

{{% server_upgrade_duration %}}

## Overview

These instructions cover the process of upgrading a Chef Backend cluster. Please use to the version of Chef Backend already running on your system and your target upgrade version.:

## Update Chef Backend

The minor version update is appropriate for all upgrades of a Chef
Backend cluster within a version. For example, updating from 1.3 to 1.4 or 2.3 to 2.4.

### Prerequisite

Download the Chef Backend package for your target upgrade version and copy it onto each of the nodes in your system.

### Step 1: Block Failover

Prevent the cluster from failing-over to a follower node while during its upgrade. Start by disabling failover:

1. Run `chef-backend-ctl set-cluster-failover off`

### Step 2: Update the followers

Update the followers sequentially. Upgrading them simultaneously is not supported and may result in data loss. Verify the successful rejoin after each upgrade.

1. Install the new Chef Backend package:

    RHEL and CentOS:

    ```bash
    yum install PATH_TO_FILE.rpm
    ```

    Debian and Ubuntu:

    ```bash
    dpkg -i PATH_TO_FILE.deb
    ```

    You may also want to look at the [chef-ingredient cookbook](https://supermarket.chef.io/cookbooks/chef-ingredient) to automate downloading and installing the latest package.

2. Run the upgrade command:

    ```bash
    chef-backend-ctl upgrade
    ```

The update command will make any changes necessary to start the new service and verify that the updated node has rejoined the cluster.

Repeat the previous steps in this section for each remaining follower.

### Step 3: Update the leader

Unblock failover, trigger failover, block it again.

```bash
% chef-backend-ctl set-cluster-failover on
% chef-backend-ctl upgrade --failover
% chef-backend-ctl set-cluster-failover off
```

### Step 4: Re-enable failover

Allow failover again:

```bash
chef-backend-ctl set-cluster-failover on
```

### Step 5: Verify the cluster is stable

Check the status of the cluster:

```bash
chef-backend-ctl status
```

## Chef Backend 1.x to 2.x Upgrade

{{< warning >}}

Upgrading from Chef Backend 1.x to Chef Backend 2.x requires full cluster downtime.

{{< /warning >}}

1. Identify the node with the **leader** role using the `chef-backend-ctl cluster-status` command:

    ```none
    Name       IP              GUID                              Role      PG        ES
    backend-1  192.168.33.215  dc0c6ea77a751f94037cd950e8451fa3  leader    leader    not_master
    backend-2  192.168.33.216  008782c59d3628b6bb7f43556ac0c66c  follower  follower  not_master
    backend-3  192.168.33.217  1af654172b1830927a571d9a5ba7965b  follower  follower  master
    ```

    In this example, `backend-1` is the **leader** node, as indicated by its role in the **Role** column.

2. Install the new Chef Backend package on all nodes in the cluster:

    RHEL and CentOS:

    ```bash
    yum install PATH_TO_FILE.rpm
    ```

    Debian and Ubuntu:

    ```bash
    dpkg -i PATH_TO_FILE.deb
    ```

3. On the leader, run the following command to take the node down for the upgrade:

    ```bash
    chef-backend-ctl down-for-upgrade
    ```

4. Then issue the same command on the follower nodes:

    ```bash
    chef-backend-ctl down-for-upgrade
    ```

5. Upgrade on the follower nodes first:

    ```bash
    chef-backend-ctl upgrade
    ```

6. Then upgrade on the leader node:

    ```bash
    chef-backend-ctl upgrade
    ```

7. On any Chef Infra Server frontend nodes using the Chef Backend cluster upgraded in the previous steps, run:

    ```bash
    chef-server-ctl reconfigure
    ```

8. To continue the upgrades on Chef Infra Server frontend nodes using this back-end cluster, see [Upgrade Front-ends Associated with a Chef Backend Cluster]({{< relref "install_server_ha/#upgrading-chef-infra-server-on-the-frontend-machines" >}}).

## Chef Backend 2.x to 3.x Upgrade

| Running Version | Upgrade To Version | Upgrade Type |
|---------|---------|------|
| 2.x | 2.x | Rolling |
| 2.1 | 3.0 | Rolling |
| 2.0 | 3.0 | Downtime |

Upgrading from Chef Backend 2.0 to directly Chef Backend 3.0 requires full cluster downtime and is not supported.
Upgrading to Chef Backend 3.0 require first upgrading Chef Backend to version 2.1 or later.

1. Identify the node with the **leader** role using the `chef-backend-ctl cluster-status` command:

    ```none
    Name       IP              GUID                              Role      PG        ES
    backend-1  192.168.33.215  dc0c6ea77a751f94037cd950e8451fa3  leader    leader    not_master
    backend-2  192.168.33.216  008782c59d3628b6bb7f43556ac0c66c  follower  follower  not_master
    backend-3  192.168.33.217  1af654172b1830927a571d9a5ba7965b  follower  follower  master
    ```

    In this example, `backend-1` is the **leader** node, as indicated by its role in the **Role** column.

2. Install the new Chef Backend package on all nodes in the cluster:

    RHEL and CentOS:

    ```bash
    yum install PATH_TO_FILE.rpm
    ```

    Debian and Ubuntu:

    ```bash
    dpkg -i PATH_TO_FILE.deb
    ```

3. Upgrade on follower nodes first:

    ```bash
    chef-backend-ctl upgrade
    ```

4. Upgrade the leader node with the `--failover` option:

    ```bash
    chef-backend-ctl upgrade --failover
    ```

5. To continue the upgrades on Chef Infra Server frontend nodes using this back-end cluster, see [Upgrade Front-ends Associated with a Chef Backend Cluster]({{< relref "install_server_ha/#upgrading-chef-infra-server-on-the-frontend-machines" >}}).

## DRBD/Keepalived HA to Chef Backend 2.x

DRBD configurations are [End of Life Products]({{< relref "versions#end-of-life-eol" >}}) and no longer supported.

For a guide to migrating to Chef Backend from DRBD see the [Best Practices for Migrating Your Chef Server](https://blog.chef.io/2018/04/06/best-practices-for-migrating-your-chef-server/) and the associated webinar from the [Chef Blog](https://blog.chef.io/).
