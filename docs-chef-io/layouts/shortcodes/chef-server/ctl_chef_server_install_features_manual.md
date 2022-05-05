Use the `install` subcommand with the `--path` option to install the Chef Manage (`chef-manage`) add-on for Chef Infra Server.

```bash
sudo chef-server-ctl install PACKAGE_NAME --path /path/to/package/directory
```

For example:

```bash
sudo chef-server-ctl install chef-manage --path /root/packages
```

The `chef-server-ctl` command will install the first `chef-manage`
package found in the `/root/packages` directory.
