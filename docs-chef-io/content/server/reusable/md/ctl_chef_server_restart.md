The `restart` subcommand is used to restart all services enabled on the
Chef Infra Server or to restart an individual service by specifying the
name of that service in the command.

{{< warning >}}

When running the Chef Infra Server in a high availability configuration,
restarting all services may trigger failover.

{{< /warning >}}

This subcommand has the following syntax:

```bash
chef-server-ctl restart SERVICE_NAME
```

where `SERVICE_NAME` represents the name of any service that is listed
after running the `service-list` subcommand. When a service is
successfully restarted the output should be similar to:

```bash
ok: run: service_name: (pid 12345) 1s
```
