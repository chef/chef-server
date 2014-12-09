#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

require 'securerandom'

# Enterprise Addon Install
# On a new install, download and install all of the Chef
# addons from package repositories. Only perform this
# action during a fresh install, and not during an upgrade.
if (!OmnibusHelper.has_been_bootstrapped? &&
    node['private_chef']['topology'] == "standalone" &&
    node['private_chef']['addons']['install'])
  include_recipe "private-chef::add_ons_wrapper"
end

opscode_test_dir = "/opt/opscode/embedded/service/chef-server-bootstrap"
opscode_test_config_dir = "/opt/opscode/embedded/service/chef-server-bootstrap/bootstrapper-config"

template File.join(opscode_test_config_dir, "config.rb") do
  source "bootstrap-config.rb.erb"
  owner "root"
  group "root"
  mode "0600"
end

bootstrap_script = File.join(opscode_test_config_dir, "pivotal.yml")

template bootstrap_script do
  source "bootstrap-script.rb.erb"
  owner "root"
  group "root"
  mode "0600"
  not_if { OmnibusHelper.has_been_bootstrapped? }
end

execute "/opt/opscode/bin/private-chef-ctl start" do
  not_if { OmnibusHelper.has_been_bootstrapped? }
  retries 20
end

execute "bootstrap-platform" do
  command "/opt/opscode/embedded/bin/bundle exec ./bin/bootstrap-platform ./bootstrapper-config/config.rb ./bootstrapper-config/pivotal.yml"
  cwd opscode_test_dir
  not_if { OmnibusHelper.has_been_bootstrapped? }
  notifies :restart, 'service[opscode-erchef]'
end

#
# Once we've bootstrapped the Enterprise Chef server
# we can delete the bootstrap script that contains
# the superuser password. Although this password cannot
# be used to authenticate with the API, it should
# nevertheless be deleted. We have elected not to
# trigger the delete from the execute resource immediately
# above so that we can ensure that bootstrap scripts from
# previous installs are also cleaned up.
#
template bootstrap_script do
  action :delete
end

file OmnibusHelper.bootstrap_sentinel_file do
  owner "root"
  group "root"
  mode "0600"
  content "You've been bootstrapped, punk. Delete me if you feel lucky. Do ya, Punk?"
end
