+++
title = "chef-server.rb Settings"
draft = false
gh_repo = "chef-server"

aliases = ["/config_rb_server.html", "/config_rb_server_14/", "/config_rb_server/"]

[menu]
  [menu.server]
    title = "chef-server.rb Settings"
    identifier = "server/configure/chef-server.rb Settings"
    parent = "server/configure"
    weight = 10
+++

{{< readfile file="content/server/reusable/md/config_rb_server_summary.md" >}}

## Use Conditions

Use a `case` statement to apply different values based on whether the setting exists on the front-end or back-end servers.
Add code to the server configuration file similar to the following:

```ruby
role_name = ChefServer['servers'][node['fqdn']]['role']
case role_name
when 'backend'
  # backend-specific configuration here
when 'frontend'
  # frontend-specific configuration here
end
```

## Recommended Settings

The following settings are typically added to the server configuration
file (no equal sign is necessary to set the value):

`api_fqdn`

:   The FQDN for the Chef Infra Server. This setting is not in the
    server configuration file by default. When added, its value should
    be equal to the FQDN for the service URI used by the Chef Infra
    Server. FQDNs must always be in lowercase.
    For example: `api_fqdn "chef.example.com"`.

`bootstrap`

:   Default value: `true`.

`ip_version`

:   Use to set the IP version: `"ipv4"` or `"ipv6"`. When set to
    `"ipv6"`, the API listens on IPv6 and front end and back end
    services communicate via IPv6 when a high availability configuration
    is used. When configuring for IPv6 in a high availability
    configuration, be sure to set the netmask on the IPv6 `backend_vip`
    attribute.

    Default value: `"ipv4"`.

`notification_email`

:   Default value: `info@example.com`.

### NGINX SSL Protocols

{{< readfile file="content/server/reusable/md/server_tuning_nginx.md" >}}

## Optional Settings

The following settings are often used for performance tuning of the Chef
Infra Server in larger installations.

{{< note >}}

{{< readfile file="content/server/reusable/md/notes_config_rb_server_must_reconfigure.md" >}}

{{< /note >}}

{{< note >}}

Review the full list of [optional settings]({{< relref "/server/config_rb_server_optional_settings" >}}) that can be added to the `chef-server.rb` file.
Many of these optional settings should not be added without first consulting with Chef support.

{{< /note >}}

### bookshelf

The following setting is often modified from the default as part of the
tuning effort for the **bookshelf** service:

`bookshelf['vip']`

:   The virtual IP address.

    Default value: `node['fqdn']`.


{{< warning >}}

{{< readfile file="content/server/reusable/md/notes_server_aws_cookbook_storage.md" >}}

{{< /warning >}}

### opscode-account

The following setting is often modified from the default as part of the
tuning effort for the **opscode-account** service:

`opscode_account['worker_processes']`

:   The number of allowed worker processes. This value should be
    increased if requests made to the **opscode-account** service are
    timing out, but only if the front-end machines have available CPU
    and RAM.

    Default value: `4`.

### opscode-erchef

The following settings are often modified from the default as part of
the tuning effort for the **opscode-erchef** service:

`opscode_erchef['db_pool_size']`

:   The number of open connections to PostgreSQL that are maintained by
    the service. If failures indicate that the **opscode-erchef**
    service ran out of connections, try increasing the
    `postgresql['max_connections']` setting. If failures persist, then
    increase this value (in small increments) and also increase the
    value for `postgresql['max_connections']`.

    Default value: `20`.

`opscode_erchef['s3_url_ttl']`

:   The amount of time (in seconds) before connections to the server
    expire. If Chef Infra Client runs are timing out, increase this
    setting to `3600`, and then adjust again if necessary.

    Default value: `900`.

`opscode_erchef['strict_search_result_acls']`

:   {{< readfile file="content/server/reusable/md/settings_strict_search_result_acls.md" >}}

#### Data Collector

The following settings are often modified from the default as part of
the tuning effort for the **data_collector** **opscode-erchef**
application:

`data_collector['http_max_count']`

:   The maximum worker count for the HTTP connection pool that is used
    by the data collector. If failures indicate that **opscode-erchef**
    application has run out of HTTP connections for the
    **data_collector** then increase this value.

    Default value: `100`.

### opscode-expander

The following setting is often modified from the default as part of the
tuning effort for the **opscode-expander** service:

`opscode_expander['nodes']`

:   **Setting EOL in Chef Infra Server 14.**

    The number of allowed worker processes. The **opscode-expander**
    service runs on the back-end and feeds data to the **opscode-solr**
    service, which creates and maintains search data used by the Chef
    Infra Server. Additional memory may be required by these worker
    processes depending on the frequency and volume of Chef Infra Client
    runs across the organization, but only if the back-end machines have
    available CPU and RAM.

    Default value: `2`.

### opscode-solr4

The following sections describe ways of tuning the **opscode-solr4**
service to improve performance around large node sizes, available
memory, and update frequencies.

#### Available Memory

Use the following configuration setting to help ensure that Apache Solr
does not run out of memory:

`opscode_solr4['heap_size']`

:   **Setting EOL in Chef Infra Server 14.**

    The amount of memory (in MBs) available to Apache Solr. If there is
    not enough memory available, search queries made by nodes to Apache
    Solr may fail. The amount of memory that must be available also
    depends on the number of nodes in the organization, the frequency of
    search queries, and other characteristics that are unique to each
    organization. In general, as the number of nodes increases, so does
    the amount of memory.

    If Apache Solr is running out of memory, the
    `/var/log/opscode/opscode-solr4/current` log file will contain a message
    similar to:

    ```bash
    SEVERE: java.lang.OutOfMemoryError: Java heap space
    ```

    The default value for `opscode_solr4['heap_size']` should work for many
    organizations, especially those with fewer than 25 nodes. For
    organizations with more than 25 nodes, set this value to 25% of system
    memory or `1024`, whichever is smaller. For very large configurations,
    increase this value to 25% of system memory or `4096`, whichever is
    smaller. This value should not exceed `8192`.


#### Large Node Sizes

The maximum field length setting for Apache Solr should be greater than
any expected node object file sizes in order for them to be successfully
added to the search index. If a node object file is greater than the
maximum field length, the node object will be indexed up to the maximum,
but the part of the file past that limit will not be indexed. If this
occurs, it will seem as if nodes disappear from the search index.

To ensure that large node file sizes are indexed properly, verify the
`nginx['client_max_body_size']` and `opscode_erchef['max_request_size']` settings.

To ensure that `nginx['client_max_body_size']` and `opscode_erchef['max_request_size']` are not part of the reasons for incomplete indexing,
set `opscode_solr4['max_field_length']` setting so that its value is greater than the expected node file sizes.

`nginx['client_max_body_size']`

:   **Setting EOL in Chef Infra Server 14.**

    The maximum accepted body size for a client request, as indicated by
    the `Content-Length` request header. When the maximum accepted body
    size is greater than this value, a `413 Request Entity Too Large`
    error is returned.

    Default value: `250m`.

`opscode_erchef['max_request_size']`

:   **Setting EOL in Chef Infra Server 14.**

    When the request body size is greater than this value, a `413 Request Entity Too Large` error is returned.

    Default value: `2000000`.

`opscode_solr4['max_field_length']`

:   **Setting EOL in Chef Infra Server 14.**

    The maximum field length (in number of tokens/terms). If a field
    length exceeds this value, Apache Solr may not be able to complete
    building the index. Default value: `100000` (increased from the
    Apache Solr default value of `10000`).

    Use the `wc` command to get the byte count of a large node object file.
    For example:

    ```bash
    wc -c NODE_NAME.json
    ```

    and then ensure there is a buffer beyond that value. For example, verify
    the size of the largest node object file:

    ```bash
    wc -c nodebsp2016.json
    ```

    which returns `154516`. Update the `opscode_solr4['max_field_length']`
    setting to have a value greater than the returned value. For example:
    `180000`.

    If you don't have a node object file available then you can get an
    approximate size of the node data by running the following command on a
    node.

    ```bash
    ohai | wc -c
    ```

#### Update Frequency

At the end of every Chef Infra Client run, the node object is saved to
the Chef Infra Server. From the Chef Infra Server, each node object is
then added to the `SOLR` search index. This process is asynchronous. By
default, node objects are committed to the search index every 60 seconds
or per 1000 node objects, whichever occurs first.

When data is committed to the Apache Solr index, all incoming updates
are blocked. If the duration between updates is too short, it is
possible for the rate at which updates are asked to occur to be faster
than the rate at which objects can be actually committed.

Use the `opscode_solr4['commit_interval']` and `opscode_solr4['max_commit_docs']` settings to improve the indexing
performance of node objects:

`opscode_solr4['commit_interval']`

:   **Setting EOL in Chef Infra Server 14.**

    The frequency (in seconds) at which node objects are added to the
    Apache Solr search index.

    Default value: `60000` (every 60 seconds).

`opscode_solr4['max_commit_docs']`

:   **Setting EOL in Chef Infra Server 14.**

    The frequency (in documents) at which node objects are added to the
    Apache Solr search index.

    Default value: `1000` (every 1000 documents).

### postgresql

The following setting is often modified from the default as part of the tuning effort for the **postgresql** service:

`postgresql['max_connections']`

:   The maximum number of allowed concurrent connections. This value should only be tuned when the `opscode_erchef['db_pool_size']` value used by the **opscode-erchef** service is modified. Default value: `350`.
    If there are more than two front end machines in a cluster, the
    `postgresql['max_connections']` setting should be increased. The
    increased value depends on the number of machines in the front end,
    but also the number of services that are running on each of these
    machines.

    -   Each front end machine always runs the **oc_bifrost** and
        **opscode-erchef** services.
    -   The Reporting add-on adds the **reporting** service.

    Each of these services requires 25 connections, above the default
    value.

    Use the following formula to help determine what the increased value
    should be:

    ```ruby
    new_value = current_value + [
                (# of front end machines - 2) * (25 * # of services)
             ]
    ```

    For example, if the current value is 350, there are four front end
    machines, and all add-ons are installed, then the formula looks
    like:

    ```ruby
    550 = 350 + [(4 - 2) * (25 * 4)]
    ```

`postgresql['sslmode']`

:   SSL encryption mode between the Chef Infra Server and PostgreSQL.

    Possible values:

    - `'disable'`
    - `'require'`

    Default value: `'disable'`.
