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

{{% config_rb_server_summary %}}

### Use Conditions

{{% config_add_condition %}}

## Recommended Settings

{{% server_tuning_general %}}

### SSL Protocols

{{% server_tuning_nginx %}}

## Optional Services Tuning

The following settings are often used to for performance tuning of the
Chef Infra Server in larger installations.

{{< note >}}

{{% notes_config_rb_server_must_reconfigure %}}

{{< /note >}}

### bookshelf

{{% server_tuning_bookshelf %}}

### opscode-erchef

{{% server_tuning_erchef %}}

### postgresql

{{% server_tuning_postgresql %}}
