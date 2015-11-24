#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

require 'securerandom'

# TODO - why is this in bootstrap?
# Enterprise Addon Install
# On a new install, download and install all of the Chef
# addons from package repositories. Only perform this
# action during a fresh install, and not during an upgrade.
if (!OmnibusHelper.has_been_bootstrapped? &&
    node['private_chef']['topology'] == "standalone" &&
    node['private_chef']['addons']['install'])
  include_recipe "private-chef::add_ons_wrapper"
end

bootstrap_script = File.join(opscode_test_config_dir, "pivotal.yml")
opscode_test_config_dir = "/opt/opscode/embedded/service/chef-server-bootstrap/bootstrapper-config"

# Delete legacy bootstrap
file File.join(opscode_test_config_dir, "config.rb") do
  action :delete
end

execute "/opt/opscode/bin/private-chef-ctl start" do
  not_if { OmnibusHelper.has_been_bootstrapped? }
  retries 20
end

file bootstrap_script do
  action :delete
end

#file "/etc/opscode/pivotal.pub" do
  #action :delete
#end

# Keeping this for now, too much relies on has_been_boostrapped?
file OmnibusHelper.bootstrap_sentinel_file do
  owner "root"
  group "root"
  mode "0600"
  content "You've been bootstrapped, punk. Delete me if you feel lucky. Do ya, Punk?"
end
