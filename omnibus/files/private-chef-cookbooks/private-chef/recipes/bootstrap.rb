#
# Author:: Adam Jacob <adam@chef.io>
# Copyright:: Copyright (c) 2011-2015 Chef Software, Inc.
#
# All Rights Reserved
#

# Enterprise Addon Install
include_recipe "private-chef::opscode-erchef"

# When we're running a new standalone install and are configured
# to install addons, do so now from the package repositories.
if (!OmnibusHelper.has_been_bootstrapped? &&
    node['private_chef']['topology'] == "standalone" &&
    node['private_chef']['addons']['install'])
  include_recipe "private-chef::add_ons_wrapper"
end

pivotal_key_path = "/etc/opscode/pivotal.pem"
if File.exists?(pivotal_key_path)
  pivotal_key = OpenSSL::PKey::RSA.new(pivotal_key_path)
else
  pivotal_key = OpenSSL::PKey::RSA.generate(2048)
end

# Setting at the top level so that we don't export this to chef-server-running.json
node.set['bootstrap']['superuser_public_key'] = pivotal_key.public_key.to_s

# Clean up original bootstrap component in existing installations.
directory "/opt/opscode/embedded/service/chef-server-bootstrap" do
  action :delete
  recursive true
  ignore_failure true
end

# These should always be running by this point, but let's be certain.
%w{postgresql oc_bifrost}.each do |service|
  execute "/opt/opscode/bin/chef-server-ctl start #{service}" do
    not_if { OmnibusHelper.has_been_bootstrapped? }
  end
end

ruby_block "bootstrap-chef-server-data" do
  block do
    ChefServerDataBootstrap.new(node).bootstrap
  end
  not_if { OmnibusHelper.has_been_bootstrapped? }
  notifies :restart, 'service[opscode-erchef]'
end

file pivotal_key_path do
  owner OmnibusHelper.new(node).ownership['owner']
  group "root"
  mode "0600"
  content pivotal_key.to_pem.to_s
  sensitive true
end

file OmnibusHelper.bootstrap_sentinel_file do
  owner "root"
  group "root"
  mode "0600"
  content "You've been bootstrapped, punk. Delete me if you feel lucky. Do ya, Punk?"
end

