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

# TODO: way to override versions?
packages = [["opscode-analytics", "1.0.0-1"],
  ["opscode-push-jobs-server", "1.1.2-1"],
  ["opscode-reporting", "1.1.5-1"],
  ["opscode-manage", "1.5.4-1"]]

case node['platform_family']
when 'debian'

  packages.each do |package_params|

    name = package_params[0]
    file = "#{name}_#{package_params[1]}_amd64.deb"

    # TODO: file installation fallback?
    # TODO: file installation directory parameter?
    apt_repository name do
      # TODO: is there a better URI for this?
      uri "https://packagecloud.io/chef/test-stable/#{file}"
#      key 'http://apt.opscode.com/packages@opscode.com.gpg.key'
      distribution node['lsb']['codename']
#      deb_src true
      trusted true
      components %w( main )
      not_if { OmnibusHelper.has_been_bootstrapped? }
    end
  end

  # Performs an apt-get update
  include_recipe 'apt::default'
 
when 'rhel'
 
  packages.each do |package_params|

    name = package_params[0]
    # TODO: replace el5 with correct version:
    file = "#{name}-#{package_params[1]}.el5.x86_64.rpm"

    # TODO: file installation fallback?
    # TODO: file installation directory parameter?
    yum_repository name do
      description name
      baseurl 'https://packagecloud.io/chef/test-stable/#{file}'
#      gpgkey 'http://apt.opscode.com/packages@opscode.com.gpg.key'
      sslverify true
#      gpgcheck true
      action :create
      not_if { OmnibusHelper.has_been_bootstrapped? }
    end
  end

else
  # TODO: probably don't want to fail?  Say, on any platform where this would have
  # to be done manually.
  raise "I don't know how to install addons for platform family: #{node['platform_family']}"
end
 
package 'opscode-manage'
