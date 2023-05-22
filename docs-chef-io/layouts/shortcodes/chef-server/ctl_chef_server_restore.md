The `restore` subcommand is used to restore Chef Infra Server data from
a backup that was created by the `backup` subcommand. This subcommand
may also be used to add Chef Infra Server data to a newly-installed
server. Do not run this command in a Chef Infra Server configuration that uses an external PostgreSQL database; [use knife ec backup](https://github.com/chef/knife-ec-backup) instead. This subcommand:

- Requires rsync installed on the Chef Infra Server before running the command
- Requires a `chef-server-ctl reconfigure` before running the command

Ideally, the restore server will have the same FQDN as the server that you backed up. If the restore server has a different FQDN, then:

1. Replace the FQDN in the `/etc/opscode/chef-server.rb`.
2. Replace the FQDN in the `/etc/opscode/chef-server-running.json`.
3. Delete the old SSL certificate, key and `-ssl.conf` file from `/var/opt/opscode/nginx/ca`.
4. If you use a CA-issued certificate instead of a self-signed certificate, copy the CA-issued certificate and key into `/var/opt/opscode/nginx/ca`.
5. Update the `/etc/chef/client.rb` file on each client to point to the new server FQDN.
6. Run `chef-server-ctl reconfigure`.
7. Run `chef-server-ctl restore`.
