#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

opscode_test_dir = "/opt/opscode/embedded/service/opscode-test"
opscode_test_config_dir = "/opt/opscode/embedded/service/opscode-test/bootstrapper-config"

template File.join(opscode_test_config_dir, "config.rb") do
  source "bootstrap-config.rb.erb"
  owner "root"
  group "root"
  mode "0600"
end

template File.join(opscode_test_config_dir, "script.rb") do
  source "bootstrap-script.rb.erb"
  owner "root"
  group "root"
  mode "0600"
end

# opscode-account, opscode-erchef, and oc_bifrost MUST be up and running to bootstrap

execute "/opt/opscode/bin/private-chef-ctl start oc_bifrost" do
  not_if { OmnibusHelper.has_been_bootstrapped? }
  retries 20
end

execute "/opt/opscode/bin/private-chef-ctl start opscode-account" do
  not_if { OmnibusHelper.has_been_bootstrapped? }
  retries 20
end

execute "/opt/opscode/bin/private-chef-ctl start opscode-erchef" do
  not_if { OmnibusHelper.has_been_bootstrapped? }
  retries 20
end

execute "bootstrap-platform" do
  command "bash -c 'echo y | /opt/opscode/embedded/bin/bundle exec ./bin/bootstrap-platform -c ./bootstrapper-config/config.rb -s ./bootstrapper-config/script.rb'"
  cwd opscode_test_dir
  not_if { OmnibusHelper.has_been_bootstrapped? }
  notifies :restart, 'service[opscode-erchef]'
end

file OmnibusHelper.bootstrap_sentinel_file do
  owner "root"
  group "root"
  mode "0600"
  content "You've been bootstrapped, punk. Delete me if you feel lucky. Do ya, Punk?"
end
