+++
title = "Upgrade 12"

date = 2022-02-04T15:30:01-08:00
draft = false

[menu]
  [menu.server]
    title = "Upgrade 12"
    identifier = "server/upgrade/upgrade_12.md"
    parent = "server/upgrade"
    weight = 30
+++

## Upgrade to 12.17.15

{{< warning >}}
Upgrade Chef Infra Server and any add-ons to compatible versions before setting `insecure_addon_compat` to `false`.
{{< /warning >}}

As of version 12.14, Chef Infra Server renders passwords inside of the `/etc/opscode` directory by default.
If you are using Chef Infra Server without add-ons, or if you are using the latest add-ons versions, you can set `insecure_addon_compat` to `false` in `/etc/opscode/chef-server.rb.
and Chef Infra Server will write all credentials to a single location.

For more information on password generation, including a list of supported add-on versions, see [Chef Infra Server Credentials Management]({{< relref "server_security/#chef-infra-server-credentials-management" >}}).

## Upgrade to 12.3.0

If you are running a Chef Infra Server release before 12.3.0, please contact Chef Support for guidance on upgrading your Chef Infra Server installation.

