The `uninstall` subcommand is used to remove the Chef Infra Server
application, but without removing any of the data. This subcommand will
shut down all services (including the `runit` process supervisor).

This subcommand has the following syntax:

```bash
chef-server-ctl uninstall
```

{{< note >}}

To revert the `uninstall` subcommand, run the `reconfigure` subcommand
(because the `start` subcommand is disabled by the `uninstall` command).

{{< /note >}}
