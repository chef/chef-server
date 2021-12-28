+++
title = "Backup and Restore a Standalone or Frontend install"
draft = false

gh_repo = "chef-server"

aliases = ["/server_backup_restore.html", "/runbook/server_backup_restore/"]

[menu]
  [menu.server]
    title = "Backup and Restore"
    identifier = "server/manage/Backup and Restore"
    parent = "server/manage"
    weight = 10
+++

Periodic backups of Chef Infra Server data are an essential part of
managing and maintaining a healthy configuration and ensuring that
important data can be restored, if required. The backup takes around
4 to 5 minutes per GB of data on a t3.2xlarge AWS EC2 instance.

## chef-server-ctl

For the majority of use cases, `chef-server-ctl backup` is the
recommended way to take backups of the Chef Infra Server. Use the
following commands for managing backups of Chef Infra Server data, and
for restoring those backups.

### backup

{{% ctl_chef_server_backup %}}

**Options**

{{% ctl_chef_server_backup_options %}}

**Syntax**

{{% ctl_chef_server_backup_syntax %}}

### restore

{{% ctl_chef_server_restore %}}

**Options**

{{% ctl_chef_server_restore_options %}}

**Syntax**

{{% ctl_chef_server_restore_syntax %}}

**Examples**

```bash
chef-server-ctl restore /path/to/tar/archive.tar.gz
```

## Backup and restore a Chef Backend install

{{% EOL_backend %}}

In a disaster recovery scenario, the backup and restore processes allow
you to restore a data backup into a newly built cluster. It is not
intended for the recovery of an individual machine in the chef-backend
cluster or for a point-in-time rollback of an existing cluster.

### Backup

Restoring your data in the case of an emergency depends on having
previously made backups of:

- the data in your Chef Backend cluster
- the configuration from your Chef server

To make backups for future use in disaster scenarios:

1.  On a follower chef-backend node, run `chef-backend-ctl backup`
2.  On a Chef Infra Server node run: `chef-server-ctl backup --config-only`
3.  Move the tar archives created in steps (1) and (2) to a long-term
    storage location.

### Restore

To restore a Chef Backend-based Chef Infra Server cluster, first we need to do restore on the backend and then on the front end:

#### Backend Restore

1.  Restoring backend will create a new cluster. Select one of the node as the leader
    and restore the backup on that node first. Provide the IP address of the node
    on which you are restoring as the argument to the `--publish_address` option.

    ```bash
    chef-backend-ctl restore --publish_address X.Y.Z.W /path/to/backup.tar.gz
    ```

2.  The file `/etc/chef-backend/chef-backend-secrets.json` will be generated 
    after restor command creates a new cluster. Make a copy of this file 
    on to the follower nodes (in the example, copied to `/tmp/chef-backend-secrets.json` ).

3.  Join follower nodes to your new Chef Backend cluster, by passing the IP address of the 
    restored leader node as the argument. Provide the IP address of the follower node which you are
    joining to the cluster as the argument to the `--publish_address` option. Also provide
    the copied `chef-backend-secrets.json` as the argument for the -s option.

    ```bash
    chef-backend-ctl join-cluster --accept-license --yes --quiet IP_OF_LEADER_NODE --publish_address IP_OF_CURRENT_FOLLOWER_NODE -s /tmp/chef-backend-secrets.json
    ```
4.  Generate the configs from the new cluster for the front end.
    ```bash
    chef-backend-ctl gen-server-config chefserver.internal > /tmp/chef-server.rb
    ```


#### Frontend Restore

1.  Restore Chef Infra Server from your backed up Infra Server configuration
    (See step 2 in the backup instructions above). Alternatively, you
    can generate new configuration for this node and reconfigure it
    using the steps found in [the installation
    instructions.]({{< relref "install_server_ha/#step-5-install-and-configure-first-frontend" >}}).

    ```bash
    chef-server-ctl restore /path/to/chef-server-backup.tar.gz
    ```

    {{< note >}}

    For the Chef Infra Server version earlier than 14.11.36, the `chef-server-ctl restore` will not work. It is best to upgrade to the version 14.11.36 or later.
    For a quick fix you can edit `/opt/opscode/embedded/lib/ruby/gems/2.7.0/gems/chef-server-ctl-1.1.0/bin/chef-server-ctl` and add the following methods
    ```bash
        # External Solr/ElasticSearch Commands
    def external_status_opscode_solr4(_detail_level)
      solr = external_services['opscode-solr4']['external_url']
      begin
        Chef::HTTP.new(solr).get(solr_status_url)
        puts "run: opscode-solr4: connected OK to #{solr}"
      rescue StandardError => e
        puts "down: opscode-solr4: failed to connect to #{solr}: #{e.message.split("\n")[0]}"
      end
    end

    def external_cleanse_opscode_solr4(perform_delete)
      log <<-EOM
Cleansing data in a remote Sol4 instance is not currently supported.
EOM
    end

    def solr_status_url
      case running_service_config('opscode-erchef')['search_provider']
      when "elasticsearch"
        "/chef"
      else
        "/admin/ping?wt=json"
      end
    end
    ```

    {{< /note >}}

2. Copy the backend generated config `/tmp/chef-server.rb`, to the front end node and replace it onto `/etc/opscode/chef-server.rb`.
   Run reconfigure to apply the changes.

   ```bash
    chef-server-ctl reconfigure
   ```

3.  Run the `reindex` command to re-populate your search index

    ```bash
    chef-server-ctl reindex --all
    ```

### Verify

We recommend periodically verifying your backup by restoring a single
Chef Backend node, a single Chef Infra Server node, and ensuring that
various knife commands and Chef Infra Client runs can successfully
complete against your backup.
