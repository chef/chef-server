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

{{< readfile file="content/server/reusable/md/config_add_condition.md" >}}

## Recommended Settings

{{< readfile file="content/server/reusable/md/server_tuning_general.md" >}}

### NGINX SSL Protocols

{{< readfile file="content/server/reusable/md/server_tuning_nginx.md" >}}

## Optional Settings

The following settings are often used for performance tuning of the Chef
Infra Server in larger installations.

{{< note >}}
{{< readfile file="content/server/reusable/md/notes_config_rb_server_must_reconfigure.md" >}}
{{< /note >}}

{{< note >}}

Review the full list of [optional
settings]({{< relref "/server/config_rb_server_optional_settings" >}}) that can be added to
the chef-server.rb file. Many of these optional settings should not be
added without first consulting with Chef support.

{{< /note >}}

### bookshelf

{{< readfile file="content/server/reusable/md/server_tuning_bookshelf.md" >}}

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

{{< readfile file="content/server/reusable/md/server_tuning_erchef.md" >}}

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

### postgresql

{{< readfile file="content/server/reusable/md/server_tuning_postgresql.md" >}}

`postgresql['sslmode']`

:   SSL encryption mode between the Chef Infra Server and PostgreSQL.

    Possible values:

    - `'disable'`
    - `'require'`

    Default value: `'disable'`.
