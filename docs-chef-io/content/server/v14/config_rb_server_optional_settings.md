+++
title = "chef-server.rb 14 Optional Settings"
gh_repo = "chef-server"
+++

{{< reusable_text_versioned file="config_rb_server_summary" >}}

## Settings

The following sections describe the various settings that are available
in the chef-server.rb file.

{{< note >}}
{{< reusable_text_versioned file="notes_config_rb_server_must_reconfigure" >}}
{{< /note >}}

### General

This configuration file has the following general settings:

`addons['install']`

:   Default value: `false`.

`addons['path']`

:   Default value: `nil`.

`addons['packages']`

:   Default value:

    ```ruby
    %w{chef-manage}
    ```

`api_version`

:   The version of the Chef Infra Server. Default value: `'12.0.0'`.

`default_orgname`

:   The `ORG_NAME` part of the `/organizations` endpoint in Chef Infra
    Server.

`flavor`

:   Default value: `'cs'`.

`fips`

:   Set to `true` to run the server in FIPS compliance mode. Set to
    `false` to force the server to run without FIPS compliance mode.
    Default: The value in the kernel configuration.

{{< note  spaces=4 >}}
Chef Infra Server versions earlier than 14.5 configured with `nginx['enable_non_ssl'] = false` and `fips = true` require `export CSC_LB_URL=https://127.0.0.1` to run the command `chef-server-ctl reindex <options>`
{{< /note >}}

`insecure_addon_compat`

:   Set to `true` to keep Chef Infra Server compatible with older add-on
    versions by rendering secrets and credentials to
    `/etc/opscode/chef-server-running.json` and other files in
    `/etc/opscode/`. When set to `false`, secrets are **only** written
    to `/etc/opscode/private-chef-secrets.json` and **not** to any other
    files. Default value: `true`.

    See [Add-on
    Compatibility]({{< relref "/server/server_security#add-on-compatibility" >}}) for the
    minimum add-on versions supporting `insecure_addon_compat false`.

`install_path`

:   The directory in which the Chef Infra Server is installed. Default
    value: `'/opt/opscode'`.

`from_email`

:   The email address from which invitations to the Chef management
    console are sent. Default value: `'"Opscode" <donotreply@chef.io>'`.

`license['nodes']`

:   The number of licensed nodes. Default value: `25`.

`license['upgrade_url']`

:   The URL to visit for more information about how to update the number
    of nodes licensed for an organization. Default value:
    `'https://www.chef.io/pricing'`.

`notification_email`

:   The email addressed to which email notifications are sent. Default
    value: `'pc-default@chef.io'`.

`role`

:   The configuration type of the Chef Infra Server. Possible values:
    `backend`, `frontend`, or `standalone`. Default value:
    `'standalone'`.

`topology`

:   The topology of the Chef Infra Server. Possible values: `manual`,
    `standalone`, and `tier`. Default value: `'standalone'`.

### bookshelf

{{< reusable_text_versioned file="server_services_bookshelf" >}}

{{< note >}}
{{< reusable_text_versioned file="notes_server_aws_cookbook_storage" >}}
{{< /note >}}

This configuration file has the following settings for `bookshelf`:

`bookshelf['access_key_id']`

:   Deprecated. Use `chef-server-ctl set-secret bookshelf access_key_id` from
    the [Secrets Management]({{< relref "/server/ctl_chef_server#secrets-management" >}})
    commands.

    The access key identifier. This may point at an external storage
    location, such as Amazon EC2. See [AWS external bookshelf
    settings]({{< relref "/server#aws-settings" >}}) for more information on configuring external bookshelf. Default value: **generated**.

`bookshelf['data_dir']`

:   The directory in which on-disk data is stored. The default value is
    the recommended value. Default value:
    `/var/opt/opscode/bookshelf/data`.

`bookshelf['dir']`

:   The working directory. The default value is the recommended value.
    Default value: `/var/opt/opscode/bookshelf`.

`bookshelf['enable']`

:   Enable a service. Default value: `true`.

`bookshelf['enable_request_logging']`

:   Use to configure request logging for the bookshelf service. Default
    value: `false`.

`bookshelf['external_url']`

:   The base URL to which the service is to return links to API
    resources. Use `:host_header` to ensure the URL is derived from the
    host header of the incoming HTTP request. Default value:
    `:host_header`.

`bookshelf['listen']`

:   The IP address on which the service is to listen. Default value:
    `127.0.0.1`.

`bookshelf['log_directory']`

:   The directory in which log data is stored. The default value is the
    recommended value. Default value: `/var/log/opscode/bookshelf`.

`bookshelf['log_rotation']`

:   The log rotation policy for this service. Log files are rotated when
    they exceed `file_maxbytes`. The maximum number of log files in the
    rotation is defined by `num_to_keep`. Default value:

    ```ruby
    { 'file_maxbytes' => 104857600, 'num_to_keep' => 10 }
    ```

`bookshelf['port']`

:   The port on which the service is to listen. Default value: `4321`.

`bookshelf['secret_access_key']`

:   Deprecated. Use `chef-server-ctl set-secret bookshelf secret_access_key`
    from the [Secrets Management]({{< relref "/server/ctl_chef_server#secrets-management" >}})
    commands.

    The secret key. This may point at an external storage location, such
    as Amazon EC2. See [AWS external bookshelf
    settings]({{< relref "/server#aws-settings" >}}) for
    more information on configuring external bookshelf. Default value:
    **generated**.

`bookshelf['storage_type']`

:   Determines where cookbooks are stored. Default value: `:filesystem`.

    In instances that require cookbooks to be stored within a SQL
    backend, such as in a high availability setup, you must set
    `storage_type` to `:sql`:

    ```ruby
    bookshelf['storage_type'] = :sql
    ```

`bookshelf['stream_download']`

:   Enable stream downloading of cookbooks. This setting (when `true`)
    typically results in improved cookbook download performance,
    especially with the memory usage of the **bookshelf** service and
    the behavior of load balancers and proxies in-between Chef Infra
    Client and the Chef Infra Server. Default value: `true`.

`bookshelf['sql_connection_user']`

:   The PostgreSQL user name in `'username@hostname'` format (e.g.
    `'bookshelf@my_postgresql.postgres.database.azure.com'`), where
    `username` would normally equal the value of `bookshelf['sql_user']`
    (default: `'bookshelf'`). This setting is **required** in an
    external Azure PostgreSQL database-as-a-service configuration. If
    set to `nil`, Chef Infra Server assumes that the database is not on
    Azure and the PostgreSQL connection will be made using the value
    specified in `bookshelf['sql_user']`. Default value: `nil`.

`bookshelf['vip']`

:   The virtual IP address. This may point at an external storage
    location, such as Amazon EC2. See [AWS external bookshelf
    settings]({{< relref "/server#aws-settings" >}}) for
    more information on configuring external bookshelf. Default value:
    `127.0.0.1`.

### bootstrap

This configuration file has the following settings for `bootstrap`:

`bootstrap['enable']`

:   Indicates whether an attempt to bootstrap the Chef Infra Server is
    made. Generally only enabled on systems that have bootstrap enabled
    via a `server` entry. Default value: `true`.

### compliance forwarding

The configuration file has the following settings for forwarding
`compliance` requests using the Chef Infra Server authentication system.

`profiles['root_url']`

:   If set, any properly signed requests arriving at
    `/organizations/ORGNAME/owners/OWNER/compliance` will be forwarded
    to this URL. This is expected to be a fully qualified resource, e.g.
    `http://compliance.example.org/owners/OWNER/compliance`.

### dark_launch

This configuration file has the following settings for `dark_launch`:

`dark_launch['actions']`

:   Enable Chef actions. Default value: `true`.

`dark_launch['add_type_and_bag_to_items']`

:   Default value: `true`.

`dark_launch['new_theme']`

:   Default value: `true`.

`dark_launch['private-chef']`

:   Default value: `true`.

`dark_launch['quick_start']`

:   Default value: `false`.

`dark_launch['reporting']`

:   Enable Reporting, which performs data collection during a Chef Infra
    Client run. Default value: `true`.

`dark_launch['sql_users']`

:   Default value: `true`.

### data_collector

This configuration file has the following settings for `data_collector`:

`data_collector['root_url']`

:   The fully qualified URL to the data collector server API. When
    present, it will enable the data collector in **opscode-erchef**.
    This also enables Chef Infra Server authenticated forwarding any properly
    signed requests arriving at `/organizations/ORGNAME/data-collector`
    to this URL with the data collector token appended. This is also
    target for requests authenticated and forwarded by the
    `/organizations/ORGNAME/data-collector` endpoint. For the forwarding
    to work correctly the `data_collector['token']` field must also be
    set. For example, if the data collector in Chef Automate is being
    used, the URI would look like:
    `http://my_automate_server.example.org/data-collector/v0/`.

`data_collector['proxy']`

:   If set to `true`, Chef Infra Server will proxy all requests sent to
    `/data-collector` to the configured Chef Automate
    `data_collector['root_url']`. Note that *this route* does not check
    the request signature and add the right data_collector token, but
    just proxies the Chef Automate endpoint **as-is**. Default value: `nil`.

`data_collector['token']`

:   Deprecated. Use `chef-server-ctl set-secret data_collector token` from
    the [Secrets Management]({{< relref "/server/ctl_chef_server#secrets-management" >}})
    commands.

    Legacy configuration for shared data collector security token. When
    configured, the token will be passed as an HTTP header named
    `x-data-collector-token` which the server can choose to accept or
    reject.

`data_collector['timeout']`

:   The amount of time (in milliseconds) before a request to the data
    collector API times out. Default value: <span
    class="title-ref">30000</span>.

`data_collector['http_init_count']`

:   The initial worker count for the HTTP connection pool that is used
    by the data collector. Default value: <span
    class="title-ref">25</span>.

`data_collector['http_max_count']`

:   The maximum worker count for the HTTP connection pool that is used
    by the data collector. Default value: <span
    class="title-ref">100</span>.

`data_collector['http_max_age']`

:   The maximum connection worker age (in seconds) for the HTTP
    connection pool that is used by the data collector. Default value:
    <span class="title-ref">"{70, sec}"</span>.

`data_collector['http_cull_interval']`

:   The maximum cull interval (in minutes) for the HTTP connection pool
    that is used by the data collector. Default value: <span
    class="title-ref">"{1, min}"</span>.

`data_collector['http_max_connection_duration']`

:   The maximum connection duration (in seconds) for the HTTP connection
    pool that is used by the data collector. Default value: <span
    class="title-ref">"{70, sec}"</span>.

`data_collector['ibrowse_options']`

:   An array of comma-separated key-value pairs of ibrowse options for
    the HTTP connection pool that is used by the data collector. Default
    value: <span class="title-ref">"\[{connect_timeout,
    10000}\]"</span>.

`data_collector['health_check']`

:   A boolean that controls whether the data collector health is
    included in the overall health at the `_status` endpoint. When set
    to `true`, Chef Infra Server will report that healthy front end Chef
    HA cluster members have failed when the <span
    class="title-ref">data_collector\['root_url'\]</span><span
    class="title-ref"> cannot be reached. As a result, the load balancer
    will remove those members from the load balancer pool. Default
    value: </span><span class="title-ref">true</span>\`.

### estatsd

This configuration file has the following settings for `estatsd`:

`estatsd['dir']`

:   The working directory. The default value is the recommended value.
    Default value: `'/var/opt/opscode/estatsd'`.

`estatsd['enable']`

:   Enable a service. Default value: `true`.

`estatsd['log_directory']`

:   The directory in which log data is stored. The default value is the
    recommended value. Default value: `'/var/log/opscode/estatsd'`.

`estatsd['port']`

:   The port on which the service is to listen. Default value: `9466`.

`estatsd['protocol']`

:   Use to send application statistics with StatsD protocol formatting.
    Set this value to `statsd` to apply StatsD protocol formatting.

`estatsd['vip']`

:   The virtual IP address. Default value: `'127.0.0.1'`.

### lb / lb_internal

This configuration file has the following settings for `lb`:

`lb['api_fqdn']`

:   The FQDN for the Chef Infra Server. FQDNs must always be in lowercase.  Default value: `node['fqdn']`.

`lb['ban_refresh_interval']`

:   Default value: `600`.

`lb['bookshelf']`

:   Default value: `127.0.0.1`.

`lb['cache_cookbook_files']`

:   Default value: `false`.

`lb['chef_max_version']`

:   The maximum version of Chef Infra Client that is allowed to access
    the Chef Infra Server via the Chef Infra Server API. Default value:
    `11`.

`lb['chef_min_version']`

:   The minimum version of Chef Infra Client that is allowed to access
    the Chef Infra Server via the Chef Infra Server API. Default value:
    `10`.

`lb['chef_server_webui']`

:   Default value: `127.0.0.1`.

`lb['debug']`

:   Default value: `false`.

`lb['enable']`

:   Enable a service. Default value: `true`.

`lb['erchef']`

:   Default value: `127.0.0.1`.

`lb['maint_refresh_interval']`

:   Default value: `600`.

`lb['redis_connection_pool_size']`

:   Default value: `250`.

`lb['redis_connection_timeout']`

:   The amount of time (in milliseconds) to wait before timing out.
    Default value: `1000`.

`lb['redis_keepalive_timeout']`

:   The amount of time (in milliseconds) to wait before timing out.
    Default value: `2000`.

`lb['upstream']['bookshelf']`

:   The default value is the recommended value. Default value:
    `[ '127.0.0.1' ]`.

`lb['upstream']['oc_bifrost']`

:   The default value is the recommended value. Default value:
    `[ '127.0.0.1' ]`.

`lb['upstream']['opscode_erchef']`

:   The default value is the recommended value. Default value:
    `[ '127.0.0.1' ]`.

`lb['upstream']['opscode_solr4']`

:   The default value is the recommended value. Default value:
    `[ '127.0.0.1' ]`.

`lb['vip']`

:   The virtual IP address. Default value: `127.0.0.1`.

`lb['web_ui_fqdn']`

:   FQDNs must always be in lowercase. Default value: `node['fqdn']`.

`lb['xdl_defaults']['503_mode']`

:   The default value is the recommended value. Default value: `false`.

`lb['xdl_defaults']['couchdb_acls']`

:   The default value is the recommended value. Default value: `true`.

`lb['xdl_defaults']['couchdb_association_requests']`

:   The default value is the recommended value. Default value: `true`.

`lb['xdl_defaults']['couchdb_associations']`

:   The default value is the recommended value. Default value: `true`.

`lb['xdl_defaults']['couchdb_containers']`

:   The default value is the recommended value. Default value: `true`.

`lb['xdl_defaults']['couchdb_groups']`

:   The default value is the recommended value. Default value: `true`.

`lb['xdl_defaults']['couchdb_organizations']`

:   The default value is the recommended value. Default value: `true`.

And for the internal load balancers:

`lb_internal['account_port']`

:   Default value: `9685`.

`lb_internal['chef_port']`

:   Default value: `9680`.

`lb_internal['enable']`

:   Default value: `true`.

`lb_internal['oc_bifrost_port']`

:   Default value: `9683`.

`lb_internal['vip']`

:   The virtual IP address. Default value: `'127.0.0.1'`.

### ldap

{{< reusable_text_versioned file="config_rb_server_settings_ldap" >}}

### nginx

This configuration file has the following settings for `nginx`:

`nginx['cache_max_size']`

:   The `max_size` parameter used by the Nginx cache manager, which is
    part of the `proxy_cache_path` directive. When the size of file
    storage exceeds this value, the Nginx cache manager removes the
    least recently used data. Default value: `5000m`.

`nginx['client_max_body_size']`

:   The maximum accepted body size for a client request, as indicated by
    the `Content-Length` request header. Default value: `250m`.

`nginx['dir']`

:   The working directory. The default value is the recommended value.
    Default value: `/var/opt/opscode/nginx`.

`nginx['enable']`

:   Enable a service. Default value: `true`.

`nginx['enable_ipv6']`

:   Enable Internet Protocol version 6 (IPv6) addresses. Default value:
    `false`.

`nginx['enable_non_ssl']`

:   Allow port 80 redirects to port 443. Set to
    `true`, to enable SSL termination by the front-end hardware load balancers for WebUI and API endpoints. Default value: `false`.

{{< note  spaces=4 >}}
Chef Infra Server versions earlier than 14.5 configured with `nginx['enable_non_ssl'] = false` and `fips = true` require `export CSC_LB_URL=https://127.0.0.1` to run the command `chef-server-ctl reindex <options>`
{{< /note >}}

`nginx['enable_stub_status']`

:   Enables the Nginx `stub_status` module. See
    `nginx['stub_status']['allow_list']`,
    `nginx['stub_status']['listen_host']`,
    `nginx['stub_status']['listen_port']`, and
    `nginx['stub_status']['location']`. Default value: `true`.

`nginx['gzip']`

:   Enable gzip compression. Default value: `on`.

`nginx['gzip_comp_level']`

:   The compression level used with gzip, from least amount of
    compression (`1`, fastest) to the most (`2`, slowest). Default
    value: `2`.

`nginx['gzip_http_version']`

:   Enable gzip depending on the version of the HTTP request. Default
    value: `1.0`.

`nginx['gzip_proxied']`

:   The type of compression used based on the request and response.
    Default value: `any`.

`nginx['gzip_types']`

:   Enable compression for the specified MIME-types. Default value:

    ```ruby
    [ 'text/plain',
      'text/css',
      'application/x-javascript',
      'text/xml', 'application/xml',
      'application/xml+rss',
      'text/javascript',
      'application/json'
      ]
    ```

`nginx['keepalive_timeout']`

:   The amount of time (in seconds) to wait for requests on a HTTP
    keepalive connection. Default value: `65`.

`nginx['log_directory']`

:   The directory in which log data is stored. The default value is the
    recommended value. Default value: `/var/log/opscode/nginx`.

`nginx['log_rotation']`

:   The log rotation policy for this service. Log files are rotated when
    they exceed `file_maxbytes`. The maximum number of log files in the
    rotation is defined by `num_to_keep`. Default value:
    `{ 'file_maxbytes' => 104857600, 'num_to_keep' => 10 }`

`nginx['log_x_forwarded_for']`

:   Log \$http_x_forwarded_for ("X-Forwarded-For") instead of
    \$remote_addr if `true`. Default value `false`.

`nginx['non_ssl_port']`

:   The port on which the WebUI and API are bound for non-SSL
    connections. Default value: `80`. Use `nginx['enable_non_ssl']` to
    enable or disable SSL redirects on this port number. Set to `false`
    to disable non-SSL connections.

`nginx['sendfile']`

:   Copy data between file descriptors when `sendfile()` is used.
    Default value: `on`.

`nginx['server_name']`

:   The FQDN for the server. FQDNs must always be in lowercase. Default value: `node['fqdn']`.

`nginx['ssl_certificate']`

:   The SSL certificate used to verify communication over HTTPS. Default
    value: `nil`.

`nginx['ssl_certificate_key']`

:   The certificate key used for SSL communication. Default value:
    `nil`.

`nginx['ssl_ciphers']`

:   The list of supported cipher suites that are used to establish a
    secure connection. To favor AES256 with ECDHE forward security, drop
    the `RC4-SHA:RC4-MD5:RC4:RSA` prefix. See [this
    link](https://www.openssl.org/docs/man1.1.1/man1/ciphers.html) for more
    information. For example:

    ```ruby
    nginx['ssl_ciphers'] = HIGH: ... :!PSK
    ```

`nginx['ssl_company_name']`

:   The name of your company. Default value: `YouCorp`.

`nginx['ssl_country_name']`

:   The country in which your company is located. Default value: `US`.

`nginx['ssl_email_address']`

:   The default email address for your company. Default value:
    `you@example.com`.

`nginx['ssl_locality_name']`

:   The city in which your company is located. Default value: `Seattle`.

`nginx['ssl_organizational_unit_name']`

:   The organization or group within your company that is running the
    Chef Infra Server. Default value: `Operations`.

`nginx['ssl_port']`

:   Default value: `443`.

`nginx['ssl_protocols']`

:   The SSL protocol versions that are enabled for the Chef Infra Server API.
    Starting with Chef Infra Server 14.3, this value defaults to `'TLSv1.2'` for
    enhanced security. Previous releases defaulted to `'TLSv1 TLSv1.1 TLSv1.2'`,
    which allowed for less secure SSL connections. TLS 1.2 is supported on
    Chef Infra Client 10.16.4 and later on Linux, Unix, and macOS, and on Chef
    Infra Client 12.8 and later on Windows. If it is necessary to support these older end-of-life
    Chef Infra Client releases, set this value to `'TLSv1.1 TLSv1.2'`.

    ```ruby
    nginx['ssl_protocols'] = 'TLSv1.2'
    ```

    Default value: `TLSv1.2`.

`nginx['ssl_state_name']`

:   The state, province, or region in which your company is located.
    Default value: `WA`.

`nginx['strict_host_header']`

:   Whether nginx should only respond to requests where the Host header
    matches one of the configured FQDNs. Default value: `false`.

`nginx['stub_status']['allow_list']`

:   The IP address on which accessing the `stub_status` endpoint is
    allowed. Default value: `["127.0.0.1"]`.

`nginx['stub_status']['listen_host']`

:   The host on which the Nginx `stub_status` module listens. Default
    value: `"127.0.0.1"`.

`nginx['stub_status']['listen_port']`

:   The port on which the Nginx `stub_status` module listens. Default
    value: `"9999"`.

`nginx['stub_status']['location']`

:   The name of the Nginx `stub_status` endpoint used to access data
    generated by the Nginx `stub_status` module. Default value:
    `"/nginx_status"`.

`nginx['tcp_nodelay']`

:   Enable the Nagle buffering algorithm. Default value: `on`.

`nginx['tcp_nopush']`

:   Enable TCP/IP transactions. Default value: `on`.

`nginx["time_format"]`

:   The time format of nginx `access.log`. Possible values : `"time_iso8601"` (ex: [2020-10-21T07:22:00+00:00]), `"time_local"` (ex: [07/Jun/2018:01:05:11 +0900]).

    Default value : `"time_iso8601"`.

    **New in Chef Infra Server 14.1**

`nginx['url']`

:   Default value: `https://#{node['fqdn']}`.

`nginx['use_implicit_hosts']`

:   Automatically add <span class="title-ref">localhost</span> and any
    local IP addresses to the configured FQDNs. Useful in combination
    with `nginx['strict_host_header']`. Default value: `true`.

`nginx['show_welcome_page']`

:   Determines whether or not the default nginx welcome page is shown.
    Default value: `true`.

`nginx['worker_connections']`

:   The maximum number of simultaneous clients. Use with
    `nginx['worker_processes']` to determine the maximum number of
    allowed clients. Default value: `10240`.

`nginx['worker_processes']`

:   The number of allowed worker processes. Use with
    `nginx['worker_connections']` to determine the maximum number of
    allowed clients. Default value: `node['cpu']['total'].to_i`.

`nginx['x_forwarded_proto']`

:   The protocol used to connect to the server. Possible values: `http`
    and `https`. This is the protocol used to connect to the Chef Infra
    Server by a Chef Infra Client or a workstation. Default value:
    `'https'`.

`nginx['hsts_max_age']`

:   Time duration in seconds till which the browser caches the `HSTS` information.
    Possible values: greater than or equal to `31536000` and less than or equal to `63072000`.
    Default value: `31536000` (1 year).

`nginx['nginx_no_root']`

:   Boolean, default `false`.  Specifies that `nginx` processes, including the `master` process, should not
    run as the `root` user on a system and will instead run as `user['username']` (defaults to `opscode`).
    **REQUIRES** that `nginx['ssl_port']` and `nginx['non_ssl_port']` options are configured to non-privileged
    ports greater than `1024` or that the local system is otherwise allowed to bind to privileged ports
    with the user `user['username']`.

    **New in Chef Infra Server 14.10*

### oc_bifrost

{{< reusable_text_versioned file="server_services_bifrost" >}}

This configuration file has the following settings for `oc_bifrost`:

`oc_bifrost['db_pool_size']`

:   The number of open connections to PostgreSQL that are maintained by
    the service. This value should be increased if failures indicate
    that the **oc_bifrost** service ran out of connections. This value
    should be tuned in conjunction with the
    `postgresql['max_connections']` setting for PostgreSQL. Default
    value: `20`.

`oc_bifrost['dir']`

:   The working directory. The default value is the recommended value.
    Default value: `/var/opt/opscode/oc_bifrost`.

`oc_bifrost['enable']`

:   Enable a service. Default value: `true`.

`oc_bifrost['enable_request_logging']`

:   Use to configure request logging for the `oc_bifrost` service.
    Default value: `true`.

`oc_bifrost['extended_perf_log']`

:   Default value: `true`.

`oc_bifrost['listen']`

:   The IP address on which the service is to listen. Default value:
    `'127.0.0.1'`.

`oc_bifrost['log_directory']`

:   The directory in which log data is stored. The default value is the
    recommended value. Default value: `/var/log/opscode/oc_bifrost`.

`oc_bifrost['log_rotation']`

:   The log rotation policy for this service. Log files are rotated when
    they exceed `file_maxbytes`. The maximum number of log files in the
    rotation is defined by `num_to_keep`. Default value:

    ```ruby
    { 'file_maxbytes' => 104857600, 'num_to_keep' => 10 }
    ```

`oc_bifrost['port']`

:   The port on which the service is to listen. Default value: `9463`.

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

`oc_bifrost['sql_password']`

:   The password for the `sql_user`. Default value: **generated**.

    To override the default value, use the [Secrets
    Management]({{< relref "/server/ctl_chef_server#secrets-management" >}})
    command: `chef-server-ctl set-secret oc_bifrost sql_password`.

`oc_bifrost['sql_ro_password']`

:   The password for the `sql_ro_user`. Default value: **generated**.

    To override the default value, use the [Secrets
    Management]({{< relref "/server/ctl_chef_server#secrets-management" >}})
    command: `chef-server-ctl set-secret oc_bifrost sql_ro_password`.

`oc_bifrost['sql_ro_user']`

:   Default value: `'bifrost_ro'`.

`oc_bifrost['sql_user']`

:   The user with permission to publish data. Default value:
    `'bifrost'`.

`oc_bifrost['superuser_id']`

:   Default value: **generated**.

`oc_bifrost['vip']`

:   The virtual IP address. Default value: `'127.0.0.1'`.

### oc_chef_authz

The **opscode-authz** service is used to handle authorization requests
from oc_erchef to oc_bifrost in the Chef Infra Server.

This configuration file has the following settings for `oc_chef_authz`:

`oc_chef_authz['http_cull_interval']`

:   Default value: `'{1, min}'`.

`oc_chef_authz['http_init_count']`

:   Default value: `25`.

`oc_chef_authz['http_max_age']`

:   Default value: `'{70, sec}'`.

`oc_chef_authz['http_max_connection_duration']`

:   Default value: `'{70, sec}'`.

`oc_chef_authz['http_max_count']`

:   Default value: `100`.

`oc_chef_authz['ibrowse_options']`

:   The amount of time (in milliseconds) to wait for a connection to be
    established. Default value: `'[{connect_timeout, 5000}]'`.

`oc_chef_authz['max_connection_request_limit']`

:   The maximum number of requests allowed per connection.
    Default value: `100`.

### oc-chef-pedant

This configuration file has the following settings for `oc-chef-pedant`:

`oc_chef_pedant['debug_org_creation']`

:   Run tests with full output. Default value: `false`.

`oc_chef_pedant['dir']`

:   The working directory. The default value is the recommended value.
    Default value:

    ```ruby
    '/var/opt/opscode/oc-chef-pedant'
    ```

`oc_chef_pedant['log_directory']`

:   The directory in which log data is stored. The default value is the
    recommended value. Default value:

    ```ruby
    '/var/log/opscode/oc-chef-pedant'
    ```

`oc_chef_pedant['log_http_requests']`

:   Log HTTP requests in a file named `http-traffic.log` that is located
    in the path specified by `log_directory`. Default value: `true`.

`oc_chef_pedant['log_rotation']`

:   The log rotation policy for this service. Log files are rotated when
    they exceed `file_maxbytes`. The maximum number of log files in the
    rotation is defined by `num_to_keep`. Default value:

    ```ruby
    { 'file_maxbytes' => 104857600, 'num_to_keep' => 10 }
    ```

### oc-id

{{< reusable_text_versioned file="server_services_oc_id" >}}

This configuration file has the following settings for `oc-id`:

`oc_id['administrators']`

:   An array of Chef Infra Server user names who may add applications to
    the identity service. For example, `['user1', 'user2']`. Default
    value: `[ ]`.

`oc_id['applications']`

:   A Hash that contains OAuth 2 application information. Default value:
    `{ }`.

    {{< readfile file="layouts/shortcodes/chef-server/config_ocid_application_hash_supermarket.md" >}}

`oc_id['db_pool_size']`

:   The number of open connections to PostgreSQL that are maintained by
    the service. Default value: `'20'`.

`oc_id['dir']`

:   The working directory. The default value is the recommended value.
    Default value: none.

`oc_id['enable']`

:   Enable a service. Default value: `true`.

`oc_id['email_from_address']`

:   Outbound email address. Defaults to the `'from_email'` value.

`oc_id['log_directory']`

:   The directory in which log data is stored. The default value is the
    recommended value. Default value: `'/var/opt/opscode/oc_id'`.

`oc_id['log_rotation']`

:   The log rotation policy for this service. Log files are rotated when
    they exceed `file_maxbytes`. The maximum number of log files in the
    rotation is defined by `num_to_keep`. Default value:

    ```ruby
    { 'file_maxbytes' => 104857600, 'num_to_keep' => 10 }
    ```

`oc_id['origin']`

:   The FQDN for the server that is sending outbound email. FQDNs must
    always be in lowercase.  Defaults to the `'api_fqdn'` value, which
    is the FQDN for the Chef Infra Server.

`oc_id['num_to_keep']`

:   The number of log files to keep. Default value: `10`.

`oc_id['port']`

:   The port on which the service is to listen. Default value: `9090`.

`oc_id['sql_connection_user']`

:   The PostgreSQL user name in `'username@hostname'` format (e.g.
    `'oc_id@my_postgresql.postgres.database.azure.com'`), where
    `username` would normally equal the value of `oc_id['sql_user']`
    (default: `'od_id'`). This setting is **required** in an external
    Azure PostgreSQL database-as-a-service configuration. If set to
    `nil`, Chef Infra Server assumes that the database is not on Azure
    and the PostgreSQL connection will be made using the value specified
    in `oc_id['sql_user']`. Default value: `nil`.

`oc_id['sql_database']`

:   The name of the database. Default value: `oc_id`.

`oc_id['sql_password']`

:   The password for the `sql_user`. Default value: **generated**.

    To override the default value, use the [Secrets
    Management]({{< relref "/server/ctl_chef_server#secrets-management" >}})
    command: `chef-server-ctl set-secret oc_id sql_password`.

`oc_id['sql_user']`

:   The user with permission to write to `sql_database`. Default value:
    `oc_id`.

`oc_id['vip']`

:   The virtual IP address. Default value: `'127.0.0.1'`.

### opscode-erchef

{{< reusable_text_versioned file="server_services_erchef" >}}

This configuration file has the following settings for `opscode-erchef`:

`opscode_erchef["allow_email_update_only_from_manage"]`

:   Set to `true`, users can only update their email from the Chef management console.
    Set to `false`, users can update their email using knife and the Chef management console.

    Default value : `false`.

    **New in Chef Infra Server 14.5**

`opscode_erchef['auth_skew']`

:   Default value: `900`.

`opscode_erchef['authz_fanout']`

:   Default value: `20`.

`opscode_erchef['authz_timeout']`

:   The amount of time (in seconds) before a request to the
    **oc_bifrost** service times out. Default value: `2000`.

`opscode_erchef['base_resource_url']`

:   The base URL to which the service is to return links to API
    resources. Use `:host_header` to ensure the URL is derived from the
    host header of the incoming HTTP request. Default value:
    `:host_header`.

`opscode_erchef['bulk_fetch_batch_size']`

:   The number of nodes that may be deserialized. Currently only applies
    to the `/search` endpoint in the Chef Infra Server API. The default
    value is the recommended value. Default value: `5`.

`opscode_erchef['cache_ttl']`

:   Default value: `3600`.

`opscode_erchef['cleanup_batch_size']`

:   Default value: `0`.

`opscode_erchef['couchdb_max_conn']`

:   Default value: `'100'`.

`opscode_erchef['db_pool_size']`

:   The number of open connections to PostgreSQL that are maintained by
    the service. Default value: `20`.

`opscode_erchef['depsolver_timeout']`

:   The amount of time (in milliseconds) to wait for cookbook dependency
    problems to be solved. Default value: `'5000'`.

`opscode_erchef['depsolver_worker_count']`

:   The number of Ruby processes for which cookbook dependency problems
    are unsolved. Use the `pgrep -fl depselector` command to verify the
    number of depsolver workers that are running. If you are seeing 503
    service unavailable errors, increase this value. Default value:
    `'5'`.

`opscode_erchef['dir']`

:   The working directory. The default value is the recommended value.
    Default value: `/var/opt/opscode/opscode-erchef`.

`opscode_erchef['enable']`

:   Enable a service. Default value: `true`.

`opscode_erchef['enable_actionlog']`

:   Use to enable Chef actions, a premium feature of the Chef Infra
    Server. Default value: `false`.

`opscode_erchef['enable_request_logging']`

:   Use to configure request logging for the `opscode_erchef` service.
    Default value: `true`.

`opscode_erchef['ibrowse_max_pipeline_size']`

:   Default value: `1`.

`opscode_erchef['ibrowse_max_sessions']`

:   Default value: `256`.

`opscode_erchef['enable_ibrowse_traces']`

:   Use to configure ibrowse logging for the `opscode_erchef` service.
    Default value: `false`.

`opscode_erchef["include_version_in_status"]`

:   Set to `true` to include `server_version` as part of the `/_status` endpoint.

    Default value : `false`.

    **New in Chef Infra Server 14.1**

`opscode_erchef['listen']`

:   The IP address on which the service is to listen. Default value:
    `127.0.0.1`.

`opscode_erchef['log_directory']`

:   The directory in which log data is stored. The default value is the
    recommended value. Default value: `/var/log/opscode/opscode-erchef`.

`opscode_erchef['log_rotation']`

:   The log rotation policy for this service. Log files are rotated when
    they exceed `file_maxbytes`. The maximum number of log files in the
    rotation is defined by `num_to_keep`. Default value:

    ```ruby
    { 'file_maxbytes' => 104857600, 'num_to_keep' => 10 }
    ```

`opscode_erchef['max_cache_size']`

:   Default value: `10000`.

`opscode_erchef['max_request_size']`

:   When the request body size is greater than this value, a
    `413 Request Entity Too Large` error is returned. Default value:
    `2000000`.

`opscode_erchef['nginx_bookshelf_caching']`

:   Whether Nginx is used to cache cookbooks. When `:on`, Nginx serves
    up the cached content instead of forwarding the request. Default
    value: `:off`.

`opscode_erchef['port']`

:   The port on which the service is to listen. Default value: `8000`.

`opscode_erchef['reindex_batch_size']`

:   The number of items to fetch from the database and send to the
    search index at a time. Default value: `10`.

`opscode_erchef['reindex_sleep_min_ms']`

:   The minimum number of milliseconds to sleep before retrying a failed
    attempt to index an item. Retries are delayed a random number of
    miliseconds between `reindex_sleep_min_ms` and
    `reindex_sleep_max_ms`. Set both this and `reindex_sleep_max_ms` to
    0 to retry without delay. Default value: `500`

`opscode_erchef['reindex_sleep_max_ms']`

:   The maximum number of milliseconds to sleep before retrying a failed
    attempt to index an item. Retries are delayed a random number of
    miliseconds between `reindex_sleep_min_ms` and
    `reindex_sleep_max_ms`. Set both this and `reindex_sleep_min_ms` to
    0 to retry without delay. Default value: `2000`

`opscode_erchef['reindex_item_retries']`

:   The number of times to retry sending an object for indexing in the
    case of failure. Default value: `3`

`opscode_erchef['root_metric_key']`

:   Default value: `chefAPI`.

`opscode_erchef['s3_bucket']`

:   The name of the Amazon Simple Storage Service (S3) bucket. This may
    point at external storage locations, such as Amazon EC2. See [AWS
    external bookshelf
    settings]({{< relref "/server#aws-settings" >}}) for
    more information on configuring external bookshelf.

`opscode_erchef['s3_parallel_ops_fanout']`

:   Default value: `20`.

`opscode_erchef['s3_parallel_ops_timeout']`

:   Default value: `5000`.

`opscode_erchef['s3_url_expiry_window_size']`

:   The frequency at which unique URLs are generated. This value may be
    a specific amount of time, i.e. `15m` (fifteen minutes) or a
    percentage of the value of `s3_url_ttl`, i.e. `10%`. Default value:
    `:off`.

`opscode_erchef['s3_url_ttl']`

:   The amount of time (in seconds) before connections to the server
    expire. If node bootstraps are timing out, increase this setting.
    Default value: `28800`.

`opscode_erchef['s3_url_type']`

:   The URL style to use (`path` or `vhost`) when connecting to S3.
    Mainly used to manually override the default setting. Note that
    Amazon may eliminate path-style URLs on some or all S3 buckets
    in the future. Default value: `vhost`.

`opscode_erchef['sql_connection_user']`

:   The PostgreSQL user name in `'username@hostname'` format (e.g.
    `'opscode_chef@my_postgresql.postgres.database.azure.com'`), where
    `username` would normally equal the value of
    `opscode-erchef['sql_user']` (default: `'opscode_chef'`). This
    setting is **required** in an external Azure PostgreSQL
    database-as-a-service configuration. If set to `nil`, Chef Infra
    Server assumes that the database is not on Azure and the PostgreSQL
    connection will be made using the value specified in
    `opscode_erchef['sql_user']`.Default value: `nil`.

`opscode_erchef['strict_search_result_acls']`

:   {{< reusable_text_versioned file="settings_strict_search_result_acls" >}}

`opscode_erchef['udp_socket_pool_size']`

:   Default value: `20`.

`opscode_erchef['umask']`

:   Default value: `0022`.

`opscode_erchef['validation_client_name']`

:   Default value: `chef-validator`.

`opscode_erchef['vip']`

:   The virtual IP address. Default value: `127.0.0.1`.

`opscode_erchef['cbv_cache_enabled']`

:   Enable cookbook version response caching by setting this to `true`. If you frequently see
    very long response times from `cookbook_versions` when under load, this is worth enabling.
    Enabling this makes it possible for a client to receive stale results. When a cookbook is updated
    in place (without incrementing the version), and the old response has not expired from the cache,
    the Infra Server will give the old response to the client. Subsequent client runs will receive the
    updated response. Default value: `false`.

`opscode_erchef['cbv_cache_item_ttl']`

:   The maximum time in milliseconds that Chef Infra Server will keep any given cookbook version response in the cache when
    when `cbv_cache_enabled` is enabled.
    Default value: `30000`.

{{< note >}}
Be careful if increasing this number - requests for a given set of cookbook versions will be stale if the resolved cookbook versions are updated before the cache entry times out. This will
not occur if you increment the version of a cookbook with every cookbook update, which is the recommended approach to updating cookbooks.
{{< /note >}}

`opscode_erchef['search_queue_mode']`

: The search index queue mode . Default value: `batch`.

`opscode_erchef['search_provider']`

: The search index provider. Default value: `elasticsearch`.

`opscode_erchef['search_auth_username']`

: The OpenSearch username. Default value: `opensearch_user`.

`opscode_erchef['search_auth_password']`

: The OpenSearch password. Default value: `nil`.

### OpenSearch

This configuration file has the following settings for `opensearch`:

`opensearch['enable']`

: Enable the service. Default value: `true`.

`opensearch['dir']`

: The working directory.  The default value is the recommended value. Default value: `/var/opt/opscode/opensearch`

`opensearch['data_dir']`

: The paths used to store data. Default value: `/var/opt/opscode/elasticsearch/data`

`opensearch['plugins_directory']`

: The default location of the plugins directory depends on which package you install. Default value: `/var/opt/opscode/opensearch/plugins`

`opensearch['scripts_directory']`

: The default location of the scripts directory depends on which package you install. Default value: `/var/opt/opscode/opensearch/scripts`

`opensearch['temp_directory']`

: By default, OpenSearch uses a private temporary directory that the startup script creates immediately below the system temporary directory. Default value: `/var/opt/opscode/opensearch/tmp`

`opensearch['log_directory']`

: The directory in which log data is stored. The default value is the recommended value. Default value: `/var/log/opscode/opensearch`

`opensearch['log_rotation']['file_maxbytes']`

: The log rotation policy for this service. Log files are rotated when they exceed `file_maxbytes`. Default value: `104857600`.

`opensearch['log_rotation']['num_to_keep']`

: The log rotation policy for this service. `num_to_keep` specifies the maximum number of log files in the rotation. Default value: `10`.

`opensearch['vip']`

: The virtual IP address for the machine on which Apache Solr is running. Default value: `127.0.0.1`

`opensearch['listen']`

: The IP address for the machine on which Apache Solr is running. Default value: `127.0.0.1`

`opensearch['port']`

: The port on which the service is listening. Default value: `9200`

`opensearch['enable_gc_log']`

: Enable or disable GC logging. Default value: `false`

`opensearch['initial_cluster_join_timeout']`

: Default value: `90`

`opensearch['jvm_opts']`

: Default values are set based on [JVM configuration options](https://github.com/elastic/elasticsearch/blob/6.8/distribution/src/config/jvm.options).

    {{< note spaces=4 >}}

    Each item in this list will be placed as is into the `java_opts` config file. Entries are set in chef-server.rb as:

    ```ruby
    opensearch.jvm_opts = [
    "-xoption1",
    "-xoption2",
    ...
    "optionN"
    ]
    ```

    {{< /note >}}

`opensearch['heap_size']`

: The amount of memory (in MBs) available to OpenSearch. If there is not enough memory available, search queries made by nodes to OpenSearch may fail. The amount of memory that must be available also depends on the number of nodes in the organization, the frequency of search queries, and other characteristics that are unique to each organization. In general, as the number of nodes increases, so does the amount of memory. The default value should work for many organizations with fewer than 25 nodes. For an organization with several hundred nodes, the amount of memory that is required often exceeds 3GB. Default value is is equivalent to 25% of the system memory or 1024 MB, whichever is greater.

    {{< note spaces=4 >}}

    If `heap_size` is also specified directly in `java_opts`, it will be ignored in favor of the chef-server.rb values or the defaults as calculated here. Only use chef-server.rb to set `heap_size`. It will raise an error if the system memory is less than 4 GB. This value is bounded between 1 GB - 28 GB.

    {{< /note >}}

`opensearch['new_size']`

: Defaults to the larger of 1/16th of the `heap_size` or 32 MB.

    {{< note spaces=4 >}}

    If `new_size` is also specified directly in `java_opts`, it will be ignored in favor of the chef-server.rb values or the defaults calculated here. Only use chef-server.rb to set `new_size`.

    {{< /note >}}

### External OpenSearch

`opensearch['external']`

: Enable external `opensearch` service by setting to `true`. Default value: `false`.

`opensearch['external_url']`

: The external OpenSearch URL. Example: `http://127.0.0.1:9200`. Default value: `nil`

{{< note >}}
Chef Infra Server supports OpenSearch only as an external indexing provider. You must provide values for `external` and `external_url` under this configuration.
{{< /note >}}

### Elasticsearch

This configuration file has the following settings for `elasticsearch`:

`elasticsearch['enable']`

: Enable a service. Default value: `true`.

`elasticsearch['dir']`

: The working directory. The default value is the recommended value. Default value: `/var/opt/opscode/elasticsearch`

`elasticsearch['data_dir']`

: The paths used to store data. Default value: `/var/opt/opscode/elasticsearch/data`

`elasticsearch['plugins_directory']`

: The default location of the plugins directory depends on which package you install. Default value: `/var/opt/opscode/elasticsearch/plugins`

`elasticsearch['scripts_directory']`

: The default location of the scripts directory depends on which package you install. Default value: `/var/opt/opscode/elasticsearch/scripts`

`elasticsearch['temp_directory']`

: By default, Elasticsearch uses a private temporary directory that the startup script creates immediately below the system temporary directory. Default value: `/var/opt/opscode/elasticsearch/tmp`

`elasticsearch['log_directory']`

: The directory in which log data is stored. The default value is the recommended value. Default value: `/var/log/opscode/elasticsearch`

`elasticsearch['log_rotation']['file_maxbytes']`

: The log rotation policy for this service. Log files are rotated when they exceed file_maxbytes. Default value for 'file_maxbytes': `104857600`

`elasticsearch['log_rotation']['num_to_keep']`

: The log rotation policy for this service. The maximum number of log files in the rotation is defined by num_to_keep.  Default value for 'num_to_keep': => `10`

`elasticsearch['vip']`

: The virtual IP address for the machine on which Apache Solr is running. Default value: `127.0.0.1`

`elasticsearch['listen']`

: The IP address for the machine on which Apache Solr is running. Default value: `127.0.0.1`

`elasticsearch['port']`

: The port on which the service is to listen. Default value: `9200`

`elasticsearch['enable_gc_log']`

: Enable or disable GC logging. Default value: `false`

`elasticsearch['initial_cluster_join_timeout']`

: Default value: `90`

`elasticsearch['jvm_opts']`

: Default values are set based on [JVM configuration options](https://github.com/elastic/elasticsearch/blob/6.8/distribution/src/config/jvm.options).

{{< note >}}

Each item in this list will be placed as is into the java_opts config file. Entries are set in chef-server.rb as:

```ruby
 elasticsearch.jvm_opts = [
  "-xoption1",
  "-xoption2",
  ...
  "optionN"
 ]
```

{{< /note >}}

`elasticsearch['heap_size']`

: The amount of memory (in MBs) available to Elasticsearch. If there is not enough memory available, search queries made by nodes to Elasticsearch may fail. The amount of memory that must be available also depends on the number of nodes in the organization, the frequency of search queries, and other characteristics that are unique to each organization. In general, as the number of nodes increases, so does the amount of memory. The default value should work for many organizations with fewer than 25 nodes. For an organization with several hundred nodes, the amount of memory that is required often exceeds 3GB. Default value is is equivalent to 25% of the system memory or 1024 MB, whichever is greater.

{{< note >}}

If new_size or heap_size is also specified directly in java_opts, it will be ignored in favor of the chef-server.rb values or the defaults as calculated here. Only use chef-server.rb to set heap and new sizes. Learn more about [Elasticsearch heap-size](https://www.elastic.co/guide/en/elasticsearch/reference/current/heap-size.html). It will error out if the system memory is less than 4 GB. This value is bounded between 1 GB - 28 GB.

{{< /note >}}

`elasticsearch['new_size']`

: Defaults to the larger of 1/16th the heap_size and 32 MB.

{{< note >}}

If new_size or heap_size is also specified directly in java_opts, it will be ignored in favor of the chef-server.rb values or the defaults as calculated here.  Only use chef-server.rb to set heap and new sizes. Learn more about [Elasticsearch heap-size documentation](https://www.elastic.co/guide/en/elasticsearch/reference/current/heap-size.html).

{{< /note >}}

### postgresql

{{< reusable_text_versioned file="server_services_postgresql" >}}

This configuration file has the following settings for `postgresql`:

`postgresql['checkpoint_completion_target']`

:   A completion percentage that is used to determine how quickly a
    checkpoint should finish in relation to the completion status of the
    next checkpoint. For example, if the value is `0.5`, then a
    checkpoint attempts to finish before 50% of the next checkpoint is
    done. Default value: `0.5`.

`postgresql['checkpoint_segments']`

:   The maximum amount (in megabytes) between checkpoints in log file
    segments. Default value: `3`.

`postgresql['checkpoint_timeout']`

:   The amount of time (in minutes) between checkpoints. Default value:
    `5min`.

`postgresql['checkpoint_warning']`

:   The frequency (in seconds) at which messages are sent to the server
    log files if checkpoint segments are being filled faster than their
    currently configured values. Default value: `30s`.

`postgresql['data_dir']`

:   The directory in which on-disk data is stored. The default value is
    the recommended value. Default value:
    `/var/opt/opscode/postgresql/#{node['private_chef']['postgresql']['version']}/data`.

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

`postgresql['db_superuser']`

:   Default value: `opscode-pgsql`. If `username` is set, set
    `db_superuser` to the same value.

`postgresql['db_superuser_password']`

:   Password for the DB superuser. Default value: **generated**.

    To override the default value, use the [Secrets
    Management]({{< relref "/server/ctl_chef_server#secrets-management" >}})
    command: `chef-server-ctl set-db-superuser-password`.

`postgresql['dir']`

:   The working directory. The default value is the recommended value.
    Default value:
    `/var/opt/opscode/postgresql/#{node['private_chef']['postgresql']['version']}`.

`postgresql['effective_cache_size']`

:   The size of the disk cache that is used for data files. Default
    value: 50% of available RAM.

`postgresql['enable']`

:   Enable a service. Default value: `true`.

`postgresql['home']`

:   The home directory for PostgreSQL. Default value:
    `/var/opt/opscode/postgresql`.

`postgresql['keepalives_count']`

:   The maximum number of keepalive proves that should be sent before
    dropping a connection. Default value: `2`.

`postgresql['keepalives_idle']`

:   The amount of time (in seconds) a connection must remain idle before
    keepalive probes will resume. Default value: `60`.

`postgresql['keepalives_interval']`

:   The amount of time (in seconds) between probes. Default value: `15`.

`postgresql['listen_address']`

:   The connection source to which PostgreSQL is to respond. Default
    value: `localhost`.

`postgresql['log_directory']`

:   The directory in which log data is stored. The default value is the
    recommended value. Default value:
    `/var/log/opscode/postgresql/#{node['private_chef']['postgresql']['version']}`.

`postgresql['log_min_duration_statement']`

:   When to log a slow PostgreSQL query statement. Possible values: `-1`
    (disabled, do not log any statements), `0` (log every statement), or
    an integer greater than zero. When the integer is greater than zero,
    this value is the amount of time (in milliseconds) that a query
    statement must have run before it is logged. Default value: `-1`.

`postgresql['log_rotation']`

:   The log rotation policy for this service. Log files are rotated when
    they exceed `file_maxbytes`. The maximum number of log files in the
    rotation is defined by `num_to_keep`. Default value:
    `{ 'file_maxbytes' => 104857600, 'num_to_keep' => 10 }`

`postgresql['max_connections']`

:   The maximum number of allowed concurrent connections. Default value:
    `350`.

`postgresql['md5_auth_cidr_addresses']`

:   Use instead of `trust_auth_cidr_addresses` to encrypt passwords
    using MD5 hashes. Default value: `[ '127.0.0.1/32', '::1/128' ]`.

`postgresql['port']`

:   The port on which the service is to listen. Default value: `5432`.

`postgresql['shared_buffers']`

:   The amount of memory that is dedicated to PostgreSQL for data
    caching. Default value:
    `#{(node['memory']['total'].to_i / 4) / (1024)}MB`.

`postgresql['shell']`

:   Default value: `/bin/sh`.

`postgresql['shmall']`

:   The total amount of available shared memory. Default value:
    `4194304`.

`postgresql['shmmax']`

:   The maximum amount of shared memory. Default value: `17179869184`.

`postgresql['sslmode']`

:   SSL encryption mode between the Chef Infra Server and PostgreSQL.
    Valid settings are `'disable'` and `'require'`. Default value:
    `'disable'`.

`postgresql['trust_auth_cidr_addresses']`

:   Use for clear-text passwords. See `md5_auth_cidr_addresses`. Default
    value: `'127.0.0.1/32', '::1/128'`.

`postgresql['user_path']`

:   Default value: `/opt/opscode/embedded/bin:/opt/opscode/bin:$PATH`.

`postgresql['username']`

:   The PostgreSQL account user name. Default value: `opscode-pgsql`. If
    setting this value, must set `db_superuser` to the same value.

`postgresql['version']`

:   The (currently) hardcoded version of PostgreSQL. Default value:
    `'9.2'`.

`postgresql['vip']`

:   The virtual IP address. Default value: `127.0.0.1`.

`postgresql['work_mem']`

:   The size (in megabytes) of allowed in-memory sorting. Default value:
    `8MB`.

`postgresql['pg_upgrade_timeout']`

:   The timeout value (in seconds) for PostgreSQL upgrade. Default value:
    `7200`.

### redis_lb

{{< reusable_text_versioned file="server_services_redis" >}}

This configuration file has the following settings for `redis_lb`:

`redis_lb['activerehashing']`

:   Enable active rehashing. Default value: `'no'`.

`redis_lb['aof_rewrite_min_size']`

:   The minimum size of the append-only file. Only files larger than
    this value are rewritten. Default value: `'16mb'`.

`redis_lb['aof_rewrite_percent']`

:   The size of the current append-only file, as compared to the base
    size. The append-only file is rewritten when the current file
    exceeds the base size by this value. Default value: `'50'`.

`redis_lb['appendfsync']`

:   The frequency at which the operating system writes data on-disk,
    instead of waiting for more data. Possible values: `no` (don't
    fsync, let operating system flush data), `always` (fsync after every
    write to the append-only log file), and `everysec` (fsync only once
    time per second). Default value: `'always'`.

`redis_lb['appendonly']`

:   Dump data asynchronously on-disk or to an append-only log file. Set
    to `yes` to dump data to an append-only log file. Default value:
    `'no'`.

`redis_lb['bind']`

:   Bind Redis to the specified IP address. Default value:
    `'127.0.0.1'`.

`redis_lb['data_dir']`

:   The directory in which on-disk data is stored. The default value is
    the recommended value. Default value:
    `'/var/opt/opscode/redis_lb/data'`.

`redis_lb['databases']`

:   The number of databases. Default value: `'16'`.

`redis_lb['dir']`

:   The working directory. The default value is the recommended value.
    Default value: `'/var/opt/opscode/redis_lb'`.

`redis_lb['enable']`

:   Enable a service. Default value: `true`.

`redis_lb['ha']`

:   Run the Chef Infra Server in a high availability topology. When
    `topology` is set to `ha`, this setting defaults to `true`. Default
    value: `false`.

`redis_lb['keepalive']`

:   The amount of time (in seconds) to wait for requests on a
    connection. Default value: `'60'`.

`redis_lb['log_directory']`

:   The directory in which log data is stored. The default value is the
    recommended value. Default value: `'/var/log/opscode/redis_lb'`.

`redis_lb['log_rotation']`

:   The log rotation policy for this service. Log files are rotated when
    they exceed `file_maxbytes`. The maximum number of log files in the
    rotation is defined by `num_to_keep`. Default value:

    ```ruby
    { 'file_maxbytes' => 104857600, 'num_to_keep' => 10 }
    ```

`redis_lb['loglevel']`

:   The level of logging to be stored in a log file.. Possible values:
    `debug`, `notice`, `verbose`, and `warning`. Default value:
    `'notice'`.

`redis_lb['maxmemory']`

:   The maximum amount of memory (in bytes). Default value: `'8m'`.

`redis_lb['maxmemory_policy']`

:   The policy applied when the maximum amount of memory is reached.
    Possible values: `allkeys-lru` (remove keys, starting with those
    used least frequently), `allkeys-random` (remove keys randomly),
    `noeviction` (don't expire, return an error on write operation),
    `volatile-lru` (remove expired keys, starting with those used least
    frequently), `volatile-random` (remove expired keys randomly), and
    `volatile-ttl` (remove keys, starting with nearest expired time).
    Default value: `'noeviction'`.

`redis_lb['port']`

:   The port on which the service is to listen. Default value:
    `'16379'`.

`redis_lb['save_frequency']`

:   Set the save frequency. Pattern:
    `{ "seconds" => "keys", "seconds" => "keys", "seconds" => "keys" }`.
    Default value:

    ```ruby
    { '900' => '1', '300' => '10', '60' => '1000' }
    ```

    Which saves the database every 15 minutes if at least one key
    changes, every 5 minutes if at least 10 keys change, and every 60
    seconds if 10000 keys change.

`redis_lb['timeout']`

:   The amount of time (in seconds) a client may be idle before timeout.
    Default value: `'300'`.

`redis_lb['vip']`

:   The virtual IP address. Default value: `'127.0.0.1'`.

`redis_lb['password']`

:   Legacy configuration setting for the Redis password. Default value:
    **generated**.

    To override the default value, use the [Secrets
    Management]({{< relref "/server/ctl_chef_server#secrets-management" >}})
    command: `chef-server-ctl set-secret redis_lb password`.

### upgrades

This configuration file has the following settings for `upgrades`:

`upgrades['dir']`

:   The working directory. The default value is the recommended value.
    Default value: `'/var/opt/opscode/upgrades'`.

### user

This configuration file has the following settings for `user`:

`user['home']`

:   The home directory for the user under which Chef Infra Server
    services run. Default value: `/opt/opscode/embedded`.

`user['shell']`

:   The shell for the user under which Chef Infra Server services run.
    Default value: `/bin/sh`.

`user['username']`

:   The user name under which Chef Infra Server services run. Default
    value: `opscode`.

### required_recipe

`required_recipe` is a feature that allows an administrator to specify a
recipe that will be run by all Chef Infra Clients that connect to it,
regardless of the node's run list. This feature is targeted at expert
level practitioners who are delivering isolated configuration changes to
the target systems, such as self-contained agent software. Further
explanation of the feature can be found in
[Chef Infra Client Development Docs](https://github.com/chef/chef/blob/main/docs/dev/design_documents/server_enforced_recipes.md).

This configuration file has the following settings for
`required_recipe`:

`required_recipe["enable"]`

:   Whether the feature is enabled. Default value: `false`.

`required_recipe["path"]`

:   The location of the recipe to serve. The file must be owned by the
    root user and group, and may not be group or world-writeable.
    Default value: `nil`.
