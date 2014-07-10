#
# Author:: Seth Chisamore (<schisamo@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
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
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

pedant_config = File.join(pedant_etc_dir, "pedant_config.rb")
helper = OmnibusHelper.new(node)

template pedant_config do
  owner "root"
  group "root"
  mode  "0755"
  variables({
    :api_url  => node['private_chef']['nginx']['url'],
    :solr_url => "http://#{helper.vip_for_uri('opscode-solr4')}:#{node['private_chef']['opscode-solr4']['port']}",
    :opscode_account_internal_url => node['private_chef']['lb_internal']['vip'],
    :opscode_account_internal_port => node['private_chef']['lb_internal']['account_port'],
    :default_orgname => node['private_chef']['default_orgname']
  }.merge(node['private_chef']['oc-chef-pedant'].to_hash))
end
