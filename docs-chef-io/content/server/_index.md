+++
title = "Chef Infra Server Overview"
draft = false
gh_repo = "chef-server"
aliases = ["/server_overview.html", "/server_components.html", "/server_overview/", "/server/runbook/", "/runbook/"]

[cascade]
  product = ["server"]

[menu]
  [menu.server]
    title = "Infra Server Overview"
    identifier = "server/overview/Infra Server Overview"
    parent = "server/overview"
    weight = 10
+++

{{% chef-server/chef_server %}}

The Chef Infra Server API is written in [Erlang](http://www.erlang.org/), which is an excellent language for critical enterprise concerns like concurrency, fault-tolerance, and distributed environments. The Chef Infra Server can scale to the size of any enterprise.

{{< note >}}

The Chef Infra Server can be configured via the
`/etc/opscode/chef-server.rb` file. Whenever this file is modified, the
`chef-server-ctl reconfigure` command must be run to apply the changes.
See the [Chef Infra Server settings](/server/config_rb_server/) guide for
additional information.

{{< /note >}}

## Server Components

The following diagram shows the various components that are part of a
Chef Infra Server deployment and how they relate to one another.

{{< figure src="/images/server/server_components_14.svg" width="500" alt="Diagram of Chef Infra Server deployment.">}}

<table>
<colgroup>
<col style="width: 12%" />
<col style="width: 87%" />
</colgroup>
<thead>
<tr class="header">
<th>Component</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Clients</td>
<td>The Chef Infra Server is accessed primarily by nodes that are under management by Chef, as Chef Infra Client runs occur. It is also accessed by individuals who maintain cookbooks and policy that is stored on the Chef Infra Server, typically from a workstation. And also by individual users with credentials to Chef Infra Server components, such as the Chef management console.</td>
</tr>
<tr class="even">
<td>Load Balancer</td>
<td>Nginx is an open-source HTTP and reverse proxy server that is used as
the front-end load balancer for the Chef Infra Server. All requests to
the Chef Infra Server API are routed through Nginx.</td>
</tr>
<tr class="odd">
<td>Chef Manage</td>
<td>Chef Manage is the web interface for the Chef Infra Server, which uses the Chef Infra Server API for all communication to the Chef Infra Server.</td>
</tr>
<tr class="even">
<td>Chef Infra Server</td>
<td>{{< readfile file="layouts/shortcodes/chef-server/chef_server_component_erchef.md" >}}</td>
</tr>
<tr class="odd">
<td><p>Bookshelf</p></td>
<td><p>{{< readfile file="layouts/shortcodes/chef-server/chef_server_component_bookshelf.md" >}}</p>
<p>All cookbooks are stored in a dedicated repository.</p></td>
</tr>
<tr class="even">
<td><p>Messages</p></td>
<td><p>chef-elasticsearch wraps Elastisearch and exposes its REST API for indexing and search.
All messages are added to a dedicated search index repository.</p></td>
</tr>
<tr class="odd">
<td>PostgreSQL</td>
<td>{{< readfile file="layouts/shortcodes/chef-server/chef_server_component_postgresql.md" >}}</td>
</tr>
</tbody>
</table>

## External Cookbooks

The following diagram highlights the specific changes that occur when
cookbooks are stored at an external location, such as Amazon Simple
Storage Service (S3).

{{< figure src="/images/server/server_components_s3_14.svg" width="500" alt="Diagram showing changes when cookbooks stored at external location.">}}

The following table describes the components that are different from the
default configuration of the Chef Infra Server when cookbooks are stored
at an external location:

<table>
<colgroup>
<col style="width: 12%" />
<col style="width: 87%" />
</colgroup>
<thead>
<tr class="header">
<th>Component</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Clients</td>
<td>The Chef Infra Server will provide signed URLs for cookbook requests made by the various clients (individual users, knife requests, and from the Chef Infra Client that is installed on nodes under management by Chef Infra).</td>
</tr>
<tr class="even">
<td>Load Balancer</td>
<td>The signed URLs for cookbooks are pointed here, and then routed to cookbook storage, as required.</td>
</tr>
<tr class="odd">
<td>Chef Infra Server</td>
<td>{{< readfile file="layouts/shortcodes/chef-server/chef_server_component_erchef.md" >}}</td>
</tr>
<tr class="even">
<td><p>Amazon Simple Storage Service (S3)</p></td>
<td><p>{{< readfile file="layouts/shortcodes/chef-server/chef_server_component_bookshelf.md" >}}</p>
<p>This represents external cookbooks storage at Amazon Simple Storage Service (S3).</p></td>
</tr>
</tbody>
</table>

### AWS Settings

#### Required Settings

To configure external cookbook storage using Amazon Simple Storage
Service (S3) set the following configuration settings in the
`chef-server.rb` file and run `chef-server-ctl reconfigure`:

<table>
<colgroup>
<col style="width: 40%" />
<col style="width: 60%" />
</colgroup>
<thead>
<tr class="header">
<th>Setting</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>bookshelf['access_key_id']</code></td>
<td>The access key identifier. Default value: generated by default. Specifying this directly in the configuration file is discouraged. Please use <code>chef-server-ctl set-secret bookshelf access_key_id</code> from the <a href="/ctl_chef_server.html#ctl-chef-server-secrets-management">Secrets Management</a> commands.</td>
</tr>
<tr class="even">
<td><code>bookshelf['external_url']</code></td>
<td>The S3 URL.  Do not include the bucket name in the URL.</td>
</tr>
<tr class="odd">
<td><code>bookshelf['secret_access_key']</code></td>
<td>The secret key. Default value: generated by default. Specifying this directly in the configuration file is discouraged. Please use <code>chef-server-ctl set-secret bookshelf secret_access_key</code> from the <a href="/ctl_chef_server.html#ctl-chef-server-secrets-management">Secrets Management</a> commands.</td>
</tr>
<tr class="even">
<td><code>opscode_erchef['s3_bucket']</code></td>
<td>The name of the Amazon Simple Storage Service (S3) bucket. Default value: <code>bookshelf</code>.</td>
</tr>
<tr class="odd">
<td><code>bookshelf['vip']</code></td>
<td>The virtual IP address or host name of the Amazon Simple Service (S3) API.  Do not include the bucket name in the URL.  Default value: <code>127.0.0.1</code>.</td>
</tr>
<tr class="even">
<td><code>bookshelf['enable']</code></td>
<td>Enable bookshelf. When connecting to S3, this should normally be set to <code>false</code>. Default value: <code>true</code>.</td>
</tr>
</tbody>
</table>

An example `chef-server.rb` configuration:

```ruby
bookshelf['enable'] = false
bookshelf['vip'] = 's3.us-west-2.amazonaws.com'
bookshelf['external_url'] = 'https://s3.us-west-2.amazonaws.com'
bookshelf['access_key_id'] = '<ACCESS_ID>'
bookshelf['secret_access_key'] = '<ACCESS_KEY>'
opscode_erchef['s3_bucket'] = '<BUCKET_NAME>'
```

#### Optional Settings

The following optional settings are also available and may require
modification when using an external S3 provider:

<table>
<colgroup>
<col style="width: 40%" />
<col style="width: 60%" />
</colgroup>
<thead>
<tr class="header">
<th>Setting</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>opscode_erchef['nginx_bookshelf_caching']</code></td>
<td>Whether Nginx is used to cache cookbooks. When <code>:on</code>, Nginx serves up the cached content instead of forwarding the request. Default value: <code>:off</code>.</td>
</tr>
<tr class="even">
<td><code>opscode_erchef['s3_parallel_ops_fanout']</code></td>
<td>Default value: <code>20</code>.</td>
</tr>
<tr class="odd">
<td><code>opscode_erchef['s3_parallel_ops_timeout']</code></td>
<td>Default value: <code>5000</code>.</td>
</tr>
<tr class="even">
<td><code>opscode_erchef['s3_url_expiry_window_size']</code></td>
<td>The frequency at which unique URLs are generated. This value may be a specific amount of time, i.e. <code>15m</code> (fifteen minutes) or a percentage of the value of <code>s3_url_ttl</code>, i.e. <code>10%</code>. Default value: <code>:off</code>.</td>
</tr>
<tr class="odd">
<td><code>opscode_erchef['s3_url_ttl']</code></td>
<td>The amount of time (in seconds) before connections to the server expire. If node bootstraps are timing out, increase this setting. Default value: <code>28800</code>.</td>
</tr>
<tr class="even">
<td><code>opscode_erchef['s3_url_type']</code></td>
<td> The URL style to use (<code>path</code> or <code>vhost</code>) when connecting to S3. Mainly used to manually override the default setting. Note that Amazon may eliminate path-style URLs on some or all S3 buckets in the future. Default value: <code>vhost</code>.</td>
</tr>
</tbody>
</table>

## External PostgreSQL

The following diagram highlights the specific changes that occur when
PostgreSQL is configured and managed independently of the Chef Infra
Server configuration.

{{< figure src="/images/server/server_components_postgresql_14.svg" width="500" alt="Diagram showing changes when PostgreSQL is configured and managed independently of the Chef Infra Server configuration.">}}


The following table describes the components in an external PostgreSQL
configuration that are different from the default configuration of the
Chef Infra Server:

<table>
<colgroup>
<col style="width: 12%" />
<col style="width: 87%" />
</colgroup>
<thead>
<tr class="header">
<th>Component</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Chef Infra Server</td>
<td>The Chef Infra Server configuration file is updated to point to an independently configured set of servers for PostgreSQL.</td>
</tr>
<tr class="even">
<td><p>PostgreSQL</p></td>
<td><p>{{< readfile file="layouts/shortcodes/chef-server/chef_server_component_postgresql.md" >}}</p>
<p>This represents the independently configured set of servers that are running PostgreSQL and are configured to act as the data store for the Chef Infra Server.</p></td>
</tr>
</tbody>
</table>

### PostgreSQL Settings

Use the following configuration settings in the chef-server.rb file to
configure external PostgreSQL for use with the Chef Infra Server:

`postgresql['db_superuser']`

:   Required when `postgresql['external']` is set to `true`. The
    PostgreSQL user name. This user must be granted either the
    `CREATE ROLE` and `CREATE DATABASE` permissions in PostgreSQL or be
    granted `SUPERUSER` permission. This user must also have an entry in
    the host-based authentication configuration file used by PostgreSQL
    (traditionally named `pg_hba.conf`). Default value:
    `'superuser_userid'`.

`postgresql['db_superuser_password']`

:   The password for the user specified by `postgresql['db_superuser']`.
    Required when `postgresql['external']` is set to `true`.

    The db_superuser_password can also be set using
    `chef-server-ctl set-db-superuser-password` from the [Secrets
    Management](/ctl_chef_server/#ctl-chef-server-secrets-management)
    commands.

`postgresql['external']`

:   Required. Set to `true` to run PostgreSQL external to the Chef Infra
    Server. Must be set once only on a new installation of the Chef
    Infra Server before the first `chef-server-ctl reconfigure` command
    is run. If this is set after a reconfigure or set to `false`, any
    reconfigure of the Chef Infra Server will return an error. Default
    value: `false`.

`postgresql['port']`

:   Optional when `postgresql['external']` is set to `true`. The port on
    which the service is to listen. The port used by PostgreSQL if that
    port is **not** 5432. Default value: `5432`.

`postgresql['vip']`

:   Required when `postgresql['external']` is set to `true`. The virtual
    IP address. The host for this IP address must be online and
    reachable from the Chef Infra Server via the port specified by
    `postgresql['port']`. Set this value to the IP address or hostname
    for the machine on which external PostgreSQL is located when
    `postgresql['external']` is set to `true`.

#### Optional Settings

The following optional settings are required when configuring external
PostgreSQL on Microsoft Azure:

`bookshelf['sql_connection_user']`

:   The PostgreSQL user name in `'username@hostname'` format (e.g.
    `'bookshelf@my_postgresql.postgres.database.azure.com'`), where
    `username` would normally equal the value of `bookshelf['sql_user']`
    (default: `'bookshelf'`). This setting is **required** in an
    external Azure PostgreSQL database-as-a-service configuration. If
    set to `nil`, Chef Infra Server assumes that the database is not on
    Azure and the PostgreSQL connection will be made using the value
    specified in `bookshelf['sql_user']` Default value: `nil`.

`oc_bifrost['sql_connection_user']`

:   The PostgreSQL user name in `'username@hostname'` format (e.g.
    `'bifrost@my_postgresql.postgres.database.azure.com'`), where
    `username` would normally equal the value of
    `oc_bifrost['sql_user']` (default: `'bifrost'`). This setting is
    **required** in an external Azure PostgreSQL database-as-a-service
    configuration. If set to `nil`, Chef Infra Server assumes that the
    database is not on Azure and the PostgreSQL connection will be made
    using the value specified in `oc_bifrost['sql_user']`. Default
    value: `nil`.

`oc_id['sql_connection_user']`

:   The PostgreSQL user name in `'username@hostname'` format (e.g.
    `'oc_id@my_postgresql.postgres.database.azure.com'`), where
    `username` would normally equal the value of `oc_id['sql_user']`
    (default: `'od_id'`). This setting is **required** in an external
    Azure PostgreSQL database-as-a-service configuration. If set to
    `nil`, Chef Infra Server assumes that the database is not on Azure
    and the PostgreSQL connection will be made using the value specified
    in `oc_id['sql_user']`. Default value: `nil`.

`opscode_erchef['sql_connection_user']`

:   The PostgreSQL user name in `'username@hostname'` format (e.g.
    `'opscode_chef@my_postgresql.postgres.database.azure.com'`), where
    `username` would normally equal the value of
    `opscode-erchef['sql_user']` (default: `'opscode_chef'`). This
    setting is **required** in an external Azure PostgreSQL
    database-as-a-service configuration. If set to `nil`, Chef Infra
    Server assumes that the database is not on Azure and the PostgreSQL
    connection will be made using the value specified in
    `opscode_erchef['sql_user']`. Default value: `nil`.

`postgresql['db_connection_superuser']`

:   The PostgreSQL superuser name in `'username@hostname'` format (e.g.
    `'opscode_pgsql@my_postgresql.postgres.database.azure.com'`), where
    `username` would normally equal the value of
    `postgresql['db_superuser']` with any dashes replaced by
    underscores. This setting is **required** in an external Azure
    PostgreSQL database-as-a-service configuration. If set to `nil`,
    Chef Infra Server assumes that the database is not on Azure and the
    PostgreSQL connection will be made using the value specified in
    `postgresql['db_superuser']`. Default value: `nil`.

An example `chef-server.rb` configuration for External PostgreSQL on
Microsoft Azure:

```ruby
topology 'standalone'
postgresql['external'] = true
postgresql['vip'] = 'my_postgresql.postgres.database.azure.com'
postgresql['db_superuser'] = 'opscode_pgsql'
postgresql['db_superuser_password'] = 'My_postgres_password1!'
postgresql['db_connection_superuser'] = 'opscode_pgsql@my_postgresql.postgres.database.azure.com'
# postgresql['sslmode']='require' # required if 'Enforce SSL connection' is enabled on Azure PostgreSQL
bookshelf['sql_connection_user'] = 'bookshelf@my_postgresql.postgres.database.azure.com'
oc_bifrost['sql_connection_user'] = 'bifrost@my_postgresql.postgres.database.azure.com'
oc_id['sql_connection_user'] = 'oc_id@my_postgresql.postgres.database.azure.com'
opscode_erchef['sql_connection_user'] = 'opscode_chef@my_postgresql.postgres.database.azure.com'
```

{{< note >}}

See the list of [error messages that may be
present](/errors/#external-postgresql) when configuring the Chef
Infra Server to use a remote PostgreSQL server.

{{< /note >}}

### Bookshelf Settings

In instances that require cookbooks to be stored within a SQL backend,
such as in a high availability setup, you must set the `storage_type` to
`:sql`:

```ruby
bookshelf['storage_type'] = :sql
```
