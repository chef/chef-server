+++
title = "Server Tuning"
draft = false

gh_repo = "chef-server"

aliases = ["/server_tuning.html", "/runbook/server_tuning/"]

[menu]
  [menu.server]
    title = "Tuning"
    identifier = "server/manage/Tuning"
    parent = "server/manage"
    weight = 40
+++

The server configuration file contains a list of all configuration
options that are available for the Chef Infra Server. Some of these
values should be modified for large-scale installations.

{{< note >}}

This topic contains general information about how settings can be tuned.
In many cases, this topic suggests specific values to be used for
tuning. That said, every organization and configuration is different, so
please don't hesitate to contact Chef support to discuss your tuning
effort so as to help ensure the right value is identified for any
particular setting.

{{< /note >}}

## Customize the Config File

{{% chef-server/config_rb_server_summary %}}

### Use Conditions

Use a `case` statement to apply different values based on whether the
setting exists on the front-end or back-end servers. Add code to the
server configuration file similar to the following:

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
    Server. For example: `api_fqdn "chef.example.com"`.

`bootstrap`

:   Default value: `true`.

`ip_version`

:   Use to set the IP version: `"ipv4"` or `"ipv6"`. When set to
    `"ipv6"`, the API listens on IPv6 and front end and back end
    services communicate using IPv6 when a high availability configuration
    is used. When configuring for IPv6 in a high availability
    configuration, be sure to set the netmask on the IPv6 `backend_vip`
    attribute. Default value: `"ipv4"`.

`notification_email`

:   Default value: `info@example.com`.

### SSL Protocols

{{% chef-server/server_tuning_nginx %}}

## Optional Services Tuning

The following settings are often used to for performance tuning of the
Chef Infra Server in larger installations.

{{< note >}}
{{% chef-server/notes_config_rb_server_must_reconfigure %}}
{{< /note >}}

### bookshelf

The following setting is often modified from the default as part of the
tuning effort for the **bookshelf** service:

`bookshelf['vip']`

:   The virtual IP address. Default value: `node['fqdn']`.

### opscode-erchef

The following settings are often modified from the default as part of
the tuning effort for the **opscode-erchef** service:

`opscode_erchef['db_pool_size']`

:   The number of open connections to PostgreSQL that are maintained by
    the service. If failures indicate that the **opscode-erchef**
    service ran out of connections, try increasing the
    `postgresql['max_connections']` setting. If failures persist, then
    increase this value (in small increments) and also increase the
    value for `postgresql['max_connections']`. Default value: `20`.

`opscode_erchef['s3_url_ttl']`

:   The amount of time (in seconds) before connections to the server
    expire. If Chef Infra Client runs are timing out, increase this
    setting to `3600`, and then adjust again if necessary. Default
    value: `900`.

`opscode_erchef['strict_search_result_acls']`

:   Use to specify that search results only return objects to which an actor
    (user, client, etc.) has read access, as determined by ACL settings.
    This affects all searches. When `true`, the performance of the Chef
    management console may increase because it enables the Chef management
    console to skip redundant ACL checks. To ensure the Chef management
    console is configured properly, after this setting has been applied with
    a `chef-server-ctl reconfigure` run `chef-manage-ctl reconfigure` to
    ensure the Chef management console also picks up the setting. Default
    value: `false`.

    <div class="admonition-warning">

    <p class="admonition-warning-title">Warning</p>

    <div class="admonition-warning-text">

    When `true`, `opscode_erchef['strict_search_result_acls']` affects all
    search results and any actor (user, client, etc.) that does not have
    read access to a search result will not be able to view it. For example,
    this could affect search results returned during a Chef Infra Client
    runs if a Chef Infra Client does not have permission to read the
    information.

    </div>

    </div>

### postgresql

The following setting is often modified from the default as part of the tuning effort for the **postgresql** service:

`postgresql['max_connections']`

:   The maximum number of allowed concurrent connections. This value should only be tuned when the `opscode_erchef['db_pool_size']` value used by the **opscode-erchef** service is modified. Default value: `350`.
    If there are more than two front end machines in a cluster, the
    `postgresql['max_connections']` setting should be increased. The
    increased value depends on the number of machines in the front end,
    but also the number of services that are running on each of these
    machines.

    - Each front end machine always runs the **oc_bifrost** and
        **opscode-erchef** services.
    - The Reporting add-on adds the **reporting** service.

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
