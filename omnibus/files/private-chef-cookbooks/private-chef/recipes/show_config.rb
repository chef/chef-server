#
# Author:: Adam Jacob (<adam@chef.io>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

if File.exists?("/etc/opscode/chef-server.rb")
	PrivateChef[:node] = node
	PrivateChef.from_file("/etc/opscode/chef-server.rb")
end
config = PrivateChef.generate_config(node['fqdn'])

puts Chef::JSONCompat.to_json_pretty(config)
