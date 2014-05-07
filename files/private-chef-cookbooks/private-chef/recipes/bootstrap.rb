#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

require 'securerandom'

opscode_test_dir = "/opt/opscode/embedded/service/opscode-test"
opscode_test_config_dir = "/opt/opscode/embedded/service/opscode-test/bootstrapper-config"

template File.join(opscode_test_config_dir, "config.rb") do
  source "bootstrap-config.rb.erb"
  owner "root"
  group "root"
  mode "0600"
end

bootstrap_script = File.join(opscode_test_config_dir, "script.rb")

template bootstrap_script do
  source "bootstrap-script.rb.erb"
  owner "root"
  group "root"
  mode "0600"
  variables({:admin_password => SecureRandom.hex(24)})
  not_if { OmnibusHelper.has_been_bootstrapped? }
end

execute "/opt/opscode/bin/private-chef-ctl start" do
  not_if { OmnibusHelper.has_been_bootstrapped? }
  retries 20
end

execute "bootstrap-platform" do
  command "bash -c 'echo y | /opt/opscode/embedded/bin/bundle exec ./bin/bootstrap-platform -c ./bootstrapper-config/config.rb -s ./bootstrapper-config/script.rb'"
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
