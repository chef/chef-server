# encoding: utf-8
# copyright: 2017, Chef Software, Inc.
# license: All rights reserved

require_relative '../libraries/helper'

title 'Chef Server Smoke Tests'

%w(
  bookshelf
  oc_bifrost
  opscode-chef-mover
  opscode-expander
  opscode-solr4
  rabbitmq
  nginx
  oc_id
  opscode-erchef
  opscode-pushy-server
  postgresql
  redis_lb
).each do |component_service|
  describe runit_service(component_service, '/opt/opscode/embedded/bin/sv') do
    it { should be_installed }
    it { should be_enabled }
    it { should be_running } unless component_service == 'opscode-chef-mover'
  end
end

# Only perform SSL verification on hosts where we know the SSL certs are
# properly configured
verify = false # TODO: switch this back after JEX-633 is complete
# verify = fetch_target_host.include?('cd.chef.co') ? false : true

chef_server_version = fetch_chef_server_version

describe http("https://#{fetch_target_host}/_status", ssl_verify: verify) do
  its('status') { should eq 200 }
end

describe json('/opt/opscode/version-manifest.json') do
  its('build_version') { should eq chef_server_version }
end
