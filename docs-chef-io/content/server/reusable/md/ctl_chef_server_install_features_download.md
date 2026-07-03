For commercial downloads, use the following URL:

```bash
https://chefdownload-commercial.chef.io/install.sh?license_id=<LICENSE_ID>
```

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
