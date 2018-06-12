#
# Author:: Adam Jacob (<adam@chef.io>)
# Copyright:: 2012-2018 Chef Software, Inc.
#
# All Rights Reserved
#

if File.exists?("/etc/opscode/chef-server.rb")
	PrivateChef[:node] = node
	PrivateChef.from_file("/etc/opscode/chef-server.rb")
end
config = PrivateChef.generate_config(node['fqdn'])

puts Chef::JSONCompat.to_json_pretty(config)
