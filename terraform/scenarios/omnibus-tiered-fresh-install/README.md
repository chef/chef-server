# Omnibus Tiered Fresh Install

This directory contains the Terraform code used to instantiate a "back-end" Chef Infra Server followed by a "front-end" Chef Infra Server utilizing an Omnibus built artifact as the install package.

Both servers receive a `/etc/opscode/chef-server.rb` configuration file that utilizes IPv6 addressing.

Once both servers are installed and configured, the pedant tests are run against the front-end.
