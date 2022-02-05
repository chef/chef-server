+++
title = "Upgrade Chef Infra Server"
draft = false

gh_repo = "chef-server"

aliases = ["/upgrade_server.html", "/upgrade_server/", "/upgrades/"]

[menu]
  [menu.server]
    title = "Upgrades"
    identifier = "server/upgrade/upgrades.md"
    parent = "server/upgrade"
    weight = 10
+++

Each new release of Chef Infra Server improves reliability and updates 3rd party components to ensure the security of the server. It is important to keep Chef Infra Server up to date to ensure the secure and reliable operation of Chef Infra in your organization.

{{< warning >}}
Before upgrading a production server make sure to upgrade a test server to confirm the process.
{{< /warning >}}

## Upgrade Matrix

If running a Chef Infra Server 12.17.15 or later you can upgrade directly to the latest releases of Chef Infra Server 14. If you are running a release before 12.17.15 you must perform a stepped upgrade as outlined below.

| Running Version | Upgrade To Version | Requires License | Supported Version |
|---------|---------|------|-----------|
| 13 | 14 | Yes | Yes |
| 12.17.15 | 14 | Yes | No |
| 12.3.0 | 12.17.15 | No | No |
| 11 | 12.3.0 | No | No |

Requires License
: Chef Infra Server 13 and later are governed by the [Chef EULA]({{< relref "chef_license" >}}). You are required to accept these terms when using Chef Infra Server for the first time by entering `Yes` when prompted.

Supported Release
: Chef Infra Server 14 and later are supported Chef Software releases. Earlier releases are not supported. For more information about supported Chef Software see the [Supported Versions]({{< relref "/versions#supported-commercial-distributions" >}}) documentation.

#### Upgrade Failure Troubleshooting

1. If the upgrade failed and you have a corrupted Chef Infra Server and/or a corrupted database, **DO NOT RISK YOUR BACKUP OF THE DATABASE.** Take all steps necessary to preserve the backup, including copying it to another disk. Consult with a professional sysadmin for instructions and best practices.

1. Contact customer support.

1. Reinstall the original version of Chef Infra Server you were using before attempting the upgrade process (if you had to perform a stepped upgrade, [install your original version of Chef Infra Server]({{< relref "install_server" >}}) before the stepped upgrade, not any versions you upgraded to in the stepped upgrade process). Again, **DO NOT RISK YOUR BACKUP OF THE DATABASE.** For example, consider using a separate disk from your backup for the new installation.

1. Consult the [restore documentation]({{< relref "server_backup_restore" >}}) and restore the database from the path to where it was saved:

   ```bash
   chef-server-ctl restore /path/to/tar/archive.tar.gz
   ```

