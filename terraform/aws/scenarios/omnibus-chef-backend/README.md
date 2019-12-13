# Omnibus Chef Backend

This directory contains the Terraform code used to instantiate three Chef Backend servers followed by a Chef Infra Server utilizing an Omnibus built artifact downloaded from `$upgrade_version_url` as the install package.

The Chef Infra Server will receive a `/etc/opscode/chef-server.rb` configuration file that is setup to use the Chef Backend servers.

Once both servers are installed and configured, the pedant tests are run against the Chef Infra Server.
