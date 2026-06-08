The `install` subcommand downloads packages from <https://chefdownload-commercial.chef.io> by default, which requires a valid `license_id`. For systems that are not behind a firewall (and have connectivity to <https://chefdownload-commercial.chef.io>), these packages can be installed as described below. For more information about commercial downloads, see <https://docs.chef.io/download/commercial/>.

1. Install add-ons

   Install Chef Manage with:

   ```bash
   sudo chef-server-ctl install chef-manage
   ```

1. Reconfigure the server

   ```bash
   sudo chef-server-ctl reconfigure
   ```

1. Reconfigure add-ons

   Reconfigure Chef Manage with:

   ```bash
   sudo chef-manage-ctl reconfigure
   ```

Finally, accept the [Chef License](https://docs.chef.io/chef_license/):

```bash
sudo chef-manage-ctl reconfigure --accept-license
```
