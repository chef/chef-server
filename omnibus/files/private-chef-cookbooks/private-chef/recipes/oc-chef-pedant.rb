#
# Author:: Seth Chisamore (<schisamo@chef.io>)
# Copyright:: 2012-2018 Chef Software, Inc.
#
# All Rights Reserved
#

pedant_dir = node['private_chef']['oc-chef-pedant']['dir']
pedant_etc_dir = File.join(pedant_dir, "etc")
pedant_log_dir = node['private_chef']['oc-chef-pedant']['log_directory']
[
  pedant_dir,
  pedant_etc_dir,
  pedant_log_dir
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

pedant_config = File.join(pedant_etc_dir, "pedant_config.rb")
helper = OmnibusHelper.new(node)


# Snag the first supported protocol version by our ruby installation
ssl_protocols = node['private_chef']['nginx']['ssl_protocols']
supported_versions = OpenSSL::SSL::SSLContext::METHODS
allowed_versions = ssl_protocols.split(/ /).select do |proto|
  supported_versions.include? proto.gsub(".", "_").to_sym
end

# In a healthy installation, we should be able to count on
# at least one shared protocol version. Leaving failure unhandled here,
# since it means that a pedant run is not possible.
ssl_version = allowed_versions.first.gsub(".", "_").to_sym
reindex_endpoint = node['private_chef']['fips_enabled'] ?
  "http://127.0.0.1" : "https://127.0.0.1"

template pedant_config do
  owner "root"
  group "root"
  mode  "0755"
  variables({
    :actions_enabled => node['private_chef']['dark_launch']['actions'],
    :api_url  => node['private_chef']['oc-chef-pedant']['chef_server'] || OmnibusHelper.new(node).nginx_ssl_url,
    :base_resource_url => node['private_chef']['opscode-erchef']['base_resource_url'],
    :solr_url => OmnibusHelper.new(node).solr_url,
    :opscode_account_internal_url => node['private_chef']['lb_internal']['vip'],
    :opscode_account_internal_port => node['private_chef']['lb_internal']['account_port'],
    :erchef_internal_vip => node['private_chef']['opscode-erchef']['vip'],
    :erchef_internal_port => node['private_chef']['opscode-erchef']['port'],
    :default_orgname => node['private_chef']['default_orgname'],
    :hostname => node['hostname'],
    :ssl_version =>  ssl_version,
    :reindex_endpoint => reindex_endpoint,
    :required_recipe_enabled => node['private_chef']['required_recipe']['enable'],
    :chef_pgsql_collector => ( node['private_chef']['postgresql']['enable'] &&
                               !node['private_chef']['postgresql']['external'] )
  }.merge(node['private_chef']['oc-chef-pedant'].to_hash))
end
