+++
title = "chef-server.rb Settings"
gh_repo = "chef-server"
+++

{{< reusable_text_versioned file="config_rb_server_summary">}}

## Use Conditions

{{< reusable_text_versioned file="config_add_condition">}}

## Recommended Settings

{{< reusable_text_versioned file="server_tuning_general">}}

### NGINX SSL Protocols

{{< reusable_text_versioned file="server_tuning_nginx">}}

## Optional Settings

The following settings are often used for performance tuning of the Chef
Infra Server in larger installations.

{{< note >}}

{{< reusable_text_versioned file="notes_config_rb_server_must_reconfigure">}}

{{< /note >}}

{{< note >}}

Review the full list of [optional
settings]({{< relref "/server/config_rb_server_optional_settings" >}}) that can be added to
the chef-server.rb file. Many of these optional settings should not be
added without first consulting with Chef support.

{{< /note >}}

### bookshelf

{{< reusable_text_versioned file="server_tuning_bookshelf">}}

{{< warning >}}

{{< reusable_text_versioned file="notes_server_aws_cookbook_storage">}}

{{< /warning >}}

### opscode-account

The following setting is often modified from the default as part of the
tuning effort for the **opscode-account** service:

`opscode_account['worker_processes']`

:   The number of allowed worker processes. This value should be
    increased if requests made to the **opscode-account** service are
    timing out, but only if the front-end machines have available CPU
    and RAM. Default value: `4`.

### opscode-erchef

{{< reusable_text_versioned file="server_tuning_erchef">}}

#### Data Collector

The following settings are often modified from the default as part of
the tuning effort for the **data_collector** **opscode-erchef**
application:

`data_collector['http_max_count']`

:   The maximum worker count for the HTTP connection pool that is used
    by the data collector. If failures indicate that **opscode-erchef**
    application has run out of HTTP connections for the
    **data_collector** then increase this value. Default value: <span
    class="title-ref">100</span>.

### postgresql

{{< reusable_text_versioned file="server_tuning_postgresql">}}

`postgresql['sslmode']`

:   SSL encryption mode between the Chef Infra Server and PostgreSQL.
    Valid settings are `'disable'` and `'require'`. Default value:
    `'disable'`.
