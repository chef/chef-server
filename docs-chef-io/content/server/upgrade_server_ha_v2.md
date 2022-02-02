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

## Prerequisite

1. Download the Chef Backend package for your target upgrade version and copy it onto each of the nodes in your system.

1. Identify the **leader** node in the back-end storage cluster:

    Find **leader** node in the **Role** column in the output of the `cluster-status` subcommand. In this example, `backend-1` is the **leader** node, as indicated by its role in the **Role** column.

    ```ruby
    chef-backend-ctl cluster-status
    ```

    ```output
    Name       IP              GUID                              Role      PG        ES
    backend-1  192.168.33.215  dc0c6ea77a751f94037cd950e8451fa3  leader    leader    not_master
    backend-2  192.168.33.216  008782c59d3628b6bb7f43556ac0c66c  follower  follower  not_master
    backend-3  192.168.33.217  1af654172b1830927a571d9a5ba7965b  follower  follower  master
    ```

## Minor Version Upgrade

The minor version update is appropriate for all upgrades of a Chef
Backend cluster within a version. For example, updating from 1.3 to 1.4 or 2.3 to 2.4.

### Step 1: Block Failover

Prevent the cluster from failing-over to a follower node while during its upgrade. Start by disabling failover, by running the following command on any one backend node:

1. `chef-backend-ctl set-cluster-failover off`

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

Unblock failover, trigger upgrade with failover, block it again.

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

1. Install the new Chef Backend package on all nodes in the cluster:

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

7. On each Chef Infra Server front-end node, apply the Chef Backend cluster upgrade configuration from the previous steps:

    ```bash
    chef-server-ctl reconfigure
    ```

8. To continue the upgrades on Chef Infra Server front-end nodes using this back-end cluster, see [Upgrade Front-ends Associated with a Chef Backend Cluster]({{< relref "install_server_ha/#upgrading-chef-infra-server-on-the-Chef Backend server-machines" >}}).

## Chef Backend Upgrade 2.x to 3.x

The Chef Backend 3.0 upgrade requires Chef Backend 2.1 or later.
Upgrade earlier versions to Chef Backend 2.1 or later. Chef Backend 2.3.19 is the latest of the version 2 releases.

| Chef Backend Version | Upgrade Target |
|:---------|:---------|
| 2.1.0--2.3.19 | 3.0.0 or later|
| 2.0.0--2.0.45 | 2.3.19 |
| 0.1.0--1.4.40| 2.3.19 |

### Planning

The versions of Chef Backend, Elasticsearch, and Chef Infra Server running your system determines the strategy, index treatment, and steps required for your upgrade.

| Chef Backend Version | Elasticsearch Version | Release Date |
|:---------|:---------|:------|:------|
| 3.0.0 | 6.8.23 | January 2022|
| 2.3.16 | 5.6.16 | January 2022|
| 2.2.0 | 5.6.16 | September 2020|
| 2.1.0 | 5.6.16 | February 2020 |
| 2.0.30 | 5.4.1 | June 2018 |
| 2.0.1 | 5.4.1 | October 2017 |
| 1.4.6 | 2.3.1 | June 2017 |

See the [Chef Backend Release Notes]({{< relref "/release_notes_backend" >}}) for more information.

#### Identify the Index Elasticsearch Version

Find the version of Elasticsearch that created the indexes. Run the following command on a back-end node and find value of he `created_string:` in the output:

```bash
curl -XGET localhost:9200/chef/_settings/index.version.created*?pretty\&human
```

#### Upgrade Strategy

The Chef Backend upgrade uses a rolling strategy, but the Elasticsearch version used to create the indexes determines if your upgrade is automatic or requires downtime.

* Indexes created with Elasticsearch 5.4.1 (`"created_string" : "5.4.1"`) or earlier require downtime.
* Indexes created with Elasticsearch 5.6.16 (`"created_string" : "5.6.16"`) or later are automatic and do not require downtime.

| Running Backend Version | Upgrade Target | Elasticsearch Version | Upgrade Strategy |
|---------|--------|------|------|
| 2.3.16 | 3.0 | `"created_string" : "5.6.16"` or higher| Automatic |
| 2.3.16 | 3.0 | lower than `"created_string" : "5.6.16"` | Downtime |
| 2.2.0 | 3.0 | `"created_string" : "5.6.16"` or higher | Automatic |
| 2.2.0 | 3.0 | lower than `"created_string" : "5.6.16"` | Downtime |
| 2.1.0 | 3.0 | `"created_string" : "5.6.16"` or higher | Automatic |
| 2.1.0 | 3.0 | lower than `"created_string" : "5.6.16"` | Automatic |
| 2.0.30 | 2.3.16 | `"created_string" : "5.4.1"` | Automatic |
| 2.0.30 | 2.3.16 | `"created_string" : "2.3.1"` | Downtime |

#### Index Treatment

The the version of Elasticsearch used to create the Chef Infra Server indexes determines the how the you will handle the indexes during the upgrade.

* Indexes created by Elasticsearch 5.4.1 (`created_string": 5.4.1`) or earlier require that you delete and then reindex after the upgrade.
* Indexes created by Elasticsearch 5.4.1 (`created_string": 5.4.1`) or earlier _and then_ upgraded to version 6.8.23 require that you delete and then reindex after the upgrade.
* Indexes created by Elasticsearch 5.6.16 (`created_string : 5.6.16`) or later will automatically reindexed.

Elasticsearch Version | Index Treatment |
|---------|--------|
|`"created_string" : "6.8.23"` | Automatic reindex |
|`"created_string" : "5.6.16"` | Automatic reindex |
| `"created_string" : "5.4.1"` | Delete and reindex |
| `"created_string" : "2.3.1"` | Delete and reindex |

### Upgrade Chef Infra Servers with Elasticsearch 2.3.1 to 5.4.1

The Chef Backend upgrade for installations with indexes created by Elasticsearch 2.3.1--5.4.1 requires downtime to delete and reindex the Chef Infra Servers.

#### Stop the Front-end Chef Infra Servers

1. Stop each the front-end Chef Infra Servers. This prevents communication between the front-end and the back-end while upgrade deletes and reindexes the server indexes.

    1. On Chef Infra Server 14.6.32 or later, put the server nodes into maintenance mode, to temporarily disable the API:

      ```bash
      sudo chef-server-ctl maintenance on
      ```

    1. On Chef Infra Server 14.6.31 or earlier, stop all server nodes with the command:

      ```bash
      sudo chef-server-ctl stop
      ```

#### Upgrade the Back-end Cluster

1. Prevent the cluster from failing-over to a follower node while during its upgrade. Start by disabling failover, by running the following command on any one backend node:

    ```bash
    chef-backend-ctl set-cluster-failover off
    ```

1. Install the new Chef Backend package on each of the nodes in the back-end storage cluster:

    RHEL and Centos:

    ```bash
    yum install PATH_TO_FILE.rpm
    ```

    Debian and Ubuntu:

    ```bash
    dpkg -i PATH_TO_FILE.deb
    ```

1. Next, run the upgrade subcommand on each of the follower nodes:

    ```bash
    chef-backend-ctl upgrade
    ```

1. After finishing the upgrades on the follower nodes, **then** upgrade the leader node, using  the `--failover` option:

    ```bash
    chef-backend-ctl set-cluster-failover on
    chef-backend-ctl upgrade --failover
    chef-backend-ctl set-cluster-failover off
    ```

1. Stop the Elasticsearch services:

  ```bash
  sudo chef-backend-ctl stop elasticsearch
  ```

1. Delete the current Elasticsearch indexes. This step ensures that Elasticsearch creates new indexes with the upgraded Elasticsearch version. Use the command:

  ```bash
  sudo rm -fr /var/opt/chef-backend/elasticsearch/data/*
  ```

1. Restart the Elasticsearch services:

  ```bash
  sudo chef-backend-ctl start elasticsearch
  ```

#### Upgrade the Index Definitions

1. Identify a front-end Chef Infra Server that does not serve requests. You can remove it from the load balancer or put in maintenance mode. If the front-end node is still in the load balancer:

   * If a front-end Chef Infra Server is still in the load balancer and meets _both_ these conditions:

      * Running version 14.6.32 or later
      * `chef-server-ctl status` runs all services

      Then you can disable the API temporarily by remote access with:

      ```bash
      sudo chef-server-ctl maintenance on
      ```

1. Run the `reconfigure` subcommand to apply the upgraded index definitions to the Chef Infra Server:

   ```bash
   sudo chef-server-ctl reconfigure
   ```

1. Restore failover:

  ```bash
  chef-backend-ctl set-cluster-failover on
  ```

#### Reindex the Chef Infra Servers

{{< note >}}
We estimate the reindexing operation will take 2 minutes for each 1000 nodes, but the it could take more time, depending on your server hardware and the complexity of your Chef data. See the [Standalone Server Upgrade](https://docs.chef.io/server/upgrades/#standalone-server) for more information.
{{< /note >}}

1. Test the process by reindexing a single organization:

   ```bash
   sudo chef-server-ctl reindex ORGNAME
   ```

   And then retrieving that organization's information:

  ```bash
  sudo /opt/opscode/bin/knife search node *:* -c /etc/opscode/pivotal.rb --server-url https://127.0.0.1:443/organizations/ORGNAME
  ```

1. If the test is successful, reindex all organizations:

   ```bash
   sudo chef-server-ctl reindex -a
   ```

#### Restore the Chef Infra Servers

1. Restore front-end Chef Infra Servers that you placed in maintenance mode to regular status:

   ```bash
   sudo chef-server-ctl maintenance off
   ```

1. Restart the services on the front-end servers:

   ```bash
   chef-server-ctl start
   ```

### Upgrade Chef Infra Servers with Elasticsearch 5.6.16

For indexes created by Elasticsearch version 5.6.16 (`"created_string" : "5.6.16"`) or later, follow these steps:

1. Identify the node with the **leader** role using the `chef-backend-ctl cluster-status` command:

    ```none
    Name       IP              GUID                              Role      PG        ES
    backend-1  192.168.33.215  dc0c6ea77a751f94037cd950e8451fa3  leader    leader    not_master
    backend-2  192.168.33.216  008782c59d3628b6bb7f43556ac0c66c  follower  follower  not_master
    backend-3  192.168.33.217  1af654172b1830927a571d9a5ba7965b  follower  follower  master
    ```

    In this example, `backend-1` is the **leader** node, as indicated by its role in the **Role** column.

1. Install the new Chef Backend package on all nodes in the cluster:

    RHEL and CentOS:

    ```bash
    yum install PATH_TO_FILE.rpm
    ```

    Debian and Ubuntu:

    ```bash
    dpkg -i PATH_TO_FILE.deb
    ```

1. Upgrade on follower nodes first:

    ```bash
    chef-backend-ctl upgrade
    ```

1. Upgrade the leader node with the `--failover` option:

    ```bash
    chef-backend-ctl upgrade --failover
    ```

1. To continue the upgrades on Chef Infra Server front-end nodes using this back-end cluster, see [Upgrade Front-ends Associated with a Chef Backend Cluster]({{< relref "install_server_ha.md/#upgrading-chef-infra-server-on-the-frontend-machines" >}}).

## DRBD/Keepalived HA to Chef Backend 2.x

DRBD configurations are [End of Life Products]({{< relref "versions.md#end-of-life-eol" >}}) and no longer supported.

For a guide to migrating to Chef Backend from DRBD see the [Best Practices for Migrating Your Chef Server](https://www.chef.io/blog/best-practices-for-migrating-your-chef-server) and the associated webinar from the [Chef Blog](https://blog.chef.io/).
