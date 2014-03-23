#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

if File.exists?("/etc/opscode/private-chef.rb")
	PrivateChef[:node] = node
	PrivateChef.from_file("/etc/opscode/private-chef.rb")
end
config = PrivateChef.generate_config(node['fqdn'])

puts Chef::JSONCompat.to_json_pretty(config)
